module Nautilus.Frontend.Parser where

import Data.Char
import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Token (GenLanguageDef(..), LanguageDef)
import qualified Text.Parsec.Token as Lex

import Nautilus.Frontend.Syntax

 ------  Language Definition ------

nautilusDef :: LanguageDef st
nautilusDef = emptyDef { commentStart    = "#{"
                       , commentEnd      = "}#"
                       , commentLine     = "#"
                       , identLetter     = alphaNum <|> char '_'
                       , opStart         = opLetter nautilusDef
                       , opLetter        = oneOf "~!%^&*-=+<>/|"
                       , reservedOpNames = [ "+", "-", "*", "/", "%", "//", "^^" --arithmetic
                                           , "<", "<=", "==", ">=", ">", "!=", "<>" --relational
                                           , "!", "&&", "||" --boolean
                                           , "~", "^", "&", "|", "<<", ">>", "<<<" --bitwise
                                           , "*", "&", "->" --memory
                                           , "=", "+=", "-=", "*=", "/=", "%=", "//=", "^^=", "^=", "&=", "|=", ">>=", "<<=", "<<<=" --assignment
                                           , "*", "&", "=>" --type
                                           ]
                       , reservedNames   = [ "import", "extern", "opaque", "intern"
                                           , "struct", "union", "bitpack", "enum", "typedef"
                                           , "open", "hide"
                                           , "val", "var", "def", "decl"
                                           , "iter", "inline", "pure", "noreturn", "vararg"
                                           , "shared", "static", "register"
                                           , "restrict", "volatile", "const"
                                           , "local"
                                           , "switch", "case", "else", "if", "elif", "else", "fallthru"
                                           , "when", "unless"
                                           , "loop", "while", "until", "break", "continue"
                                           , "for", "in", "upto", "upthru", "downto", "downthru"
                                           , "exit", "when", "case"
                                           , "return", "jump", "yield"
                                           , "label", "goto"
                                           , "pass"
                                           , "sizeof", "alignof", "typeof" --layout operators
                                           , "as", "to" --casting
                                           , "_"
                                           ]
                       }
lexer = Lex.makeTokenParser nautilusDef

nautilusNotRenamable :: [String]
nautilusNotRenamable = [ "true", "false", "null"
                       , "void", "any"
                       , "size", "diff"
                       , "byte", "ascii"
                       , "short", "ushort"
                       , "int", "uint"
                       , "long", "ulong"
                       , "float"
                       ]

--REFAC operator precedence to here (somehow)

------ Entry Point ------

------ Grammar ------

parseNautilus :: Parser [FileSyntax]
-- <nautilus> ::= <file-item>*
parseNautilus = Lex.whiteSpace lexer >> many parseFileItem

parseFileItem :: Parser FileSyntax
-- <file-item> ::= [ <visibility> ] ( <import> | <type-synonym> | <open-enum> | <value> | <static-variable> | <declaration> | <function> | <iterator> )
--              |  [ <visibility-3> ] <nominal-def>
--              |  <local-block>
parseFileItem =  try (optionMaybe parseVisibility2 >>= \vis ->
                                                    (  parseImport         (maybe Intern id vis)
                                                   <|> parseTypeSynonym    (maybe Extern id vis)
                                                   <|> parseOpenEnum       (maybe Intern id vis)
                                                   <|> parseValue          (maybe Intern id vis)
                                                   <|> parseStaticVariable (maybe Intern id vis)
                                                   <|> parseDeclaration    (maybe Extern id vis)
                                                   <|> parseFunction       (maybe Extern id vis)))
             <|> try (optionMaybe parseVisibility3 >>= parseNewType       . maybe Extern id)
             <|> try parseLocal
    where
    -- import ::= 'import' <path> [ ('open' | 'hide') <identifier> (',' <identifier>)* [','] ] ';'
    parseImport vis = do
        reserved "import"
        path  <- pathName
        open  <- option All $ parseRestriction
        semi
        return $ FImport vis path open
        where
        parseRestriction = do
            which <- (reserved "open" >> return Open) <|> (reserved "hide" >> return Hide)
            names <- identifier `sepEndBy1` comma
            return $ which names
    parseNewType vis = do
        (name, repr) <- parseNominalDefinition
        return        $ FNewType vis name repr
    -- type-synonym ::= 'typedef' <define> '=' <type> ';'
    parseTypeSynonym vis = do
        reserved "typedef"
        name  <- liftM Id definition
        oper  "="
        ty    <- parseType
        semi
        return $ FTypeSyn vis name ty
    -- open-enum ::= 'open' <identifier> ';'
    parseOpenEnum vis = do
        reserved "open"
        name  <- identifier
        semi
        return $ FOpenEnum vis name
    -- value ::= 'val' <define> [ ':' <type> ] '=' <static-expr> ';'
    parseValue vis = do
        reserved "val"
        name  <- liftM Id definition
        ty    <- optionMaybe $ colon >> parseType
        oper  "="
        expr  <- parseExpr
        semi
        return $ FVal vis name ty expr
    -- static-variable ::= [ 'var' ] <file-storage> <define> [ ':' <type> ] [ '=' <static-expr> ] ';'
    --TODO allow mulitple declarations of same type (a, b, c : int = 1, 2, 3)
    --REFAC make file and function var decls use the same code, possibly also for vals
    parseStaticVariable vis = do
        optional $ reserved "var"
        store            <- parseFileStorage
        name             <- liftM Id definition
        ty               <- optionMaybe $ colon >> parseType
        expr             <- optionMaybe $ oper "=" >> parseExpr
        semi
        return            $ FVar vis name store ty expr
    -- declaration ::= 'decl' ( <variable-decl> | <function-decl> ) ';'
    parseDeclaration vis = do
            reserved "decl"
            x     <- parseVarDecl <|> parseFuncDecl --TODO (opaque) type decl
            semi
            return   x
        where
        -- function-decl ::= <qualifier>* [ <quoted-name> ] <define> '(' [ <parameter> (',' <parameter>)* [','] ] ')' [ '=>' <type> ]
        parseFuncDecl = do
            qs       <- many parseQualifier
            m        <- optionMaybe quotedName
            name     <- liftM Id definition
            params   <- parens $ parseParameter `sepEndBy` comma
            retTy    <- option Void (oper "=>" >> parseType)
            let ty    = Function (map snd params) retTy
            return    $ FFuncDecl vis qs name m ty
        -- variable-decl ::= 'var' [ <quoted-name> ] <define> ':' <type>
        parseVarDecl = do
            reserved "var"
            m     <- optionMaybe quotedName
            name  <- liftM Id definition
            colon
            ty    <- parseType
            return $ FVarDecl vis name m ty
    -- function ::= 'def' <qualifier>* [ <quoted-name> ] <define> '(' [ <parameter> (',' <parameter>)* [','] ] ')' <compound-expr>
    parseFunction vis = do
            reserved    "def"
            qs       <- many parseQualifier
            m        <- optionMaybe quotedName
            name     <- liftM Id definition
            params   <- parens $ parseParameter `sepEndBy` comma
            retTy    <- option Void (oper "=>" >> parseType)
            let ty    = Function (map snd params) retTy
                which = if Iterator `elem` qs then InIter else InFunc
            expr     <- inProc which parseProcItem
            return    $ FFunction vis qs name m ty (params, retTy) expr
    -- qualifier ::= 'inline' | 'iter' | 'pure' | 'noreturn' | 'vararg'
    parseQualifier =  (reserved "inline"   >> return Inline)
                  <|> (reserved "iter"     >> return Iterator)
                  <|> (reserved "pure"     >> return Pure)
                  <|> (reserved "noreturn" >> return NoReturn)
                  <|> (reserved "vararg"   >> return VarArgs)
    -- <local-block> ::= 'local' '{' <file-item>* '}' 'in' '{' <file-item>* '}'
    parseLocal = do
            reserved "local"
            loc  <- braces $ many parseLocalItem
            reserved "in"
            body <- braces $ many parseLocalInItem
            return $ FLocal loc body
        where
        parseLocalItem =  try (parseTypeSynonym    Intern)
                      <|> try (parseNewType        Intern)
                      <|> try (parseOpenEnum       Intern)
                      <|> try (parseValue          Intern)
                      <|> try (parseStaticVariable Intern)
                      <|> try (parseDeclaration    Intern)
                      <|> try (parseFunction       Intern)
        parseLocalInItem = try (optionMaybe parseVisibility2 >>= \vis ->
                                                    (  parseTypeSynonym    (maybe Extern id vis)
                                                   <|> parseOpenEnum       (maybe Intern id vis)
                                                   <|> parseValue          (maybe Intern id vis)
                                                   <|> parseStaticVariable (maybe Intern id vis)
                                                   <|> parseDeclaration    (maybe Extern id vis)
                                                   <|> parseFunction       (maybe Extern id vis)))
             <|> try (optionMaybe parseVisibility3 >>= parseNewType       . maybe Extern id)

    parseParameter :: Parser (Definition, Type)
    parseParameter = do
        name  <- option Anonymous (liftM Id definition << colon)
        ty    <- parseType
        return   (name, ty)


parseVisibility2 :: Parser Visibility
parseVisibility2 =  (reserved "intern" >> return Intern)
                <|> (reserved "extern" >> return Extern)
parseVisibility3 :: Parser Visibility
parseVisibility3 =  (reserved "intern" >> return Intern)
                <|> (reserved "opaque" >> return Opaque)
                <|> (reserved "extern" >> return Extern)


parseType :: Parser Type
-- type ::= <type> <qualifier>
--       |  <type> '*'
--       |  <type> '&'
--       |  <type> '[' ']'
--       |  <type> '[' <int-expr> ']'
--       |  <param-type> '=>' <type>
--       |  <type-term>
parseType = buildExpressionParser typeOperators parseTypeTerm
    where
    typeOperators = [ [ postfix "*" Pointer
                      , postfix "&" Reference
                      , Postfix (liftM QualType parseQualifier)
                      , Postfix (try (brackets nada >> return DynArr))
                      , Postfix (try (brackets $ liftM Array parseExpr))
                      ]
                    , [ Prefix $ try $ liftM Function (parseParamType << reserved "=>") ]
                    ]
    -- param-type ::= <type> | '(' [ <type> ( ',' <type> )* ] [','] ')'
    parseParamType =  parens (parseType `sepEndBy` comma)
                  <|> liftM (:[]) parseType
    -- type-term ::= '(' <type> ')'
    --            |  <identifier>
    --            |  ( 'struct' | 'union' ) <stype-repr>
    --            |  'typeof' <expr>
    parseTypeTerm =  parens parseType
                 <|> liftM Nominal identifier
                 <|> liftM Structural (parseProductType <|> parseSumType)
                 <|> (reserved "typeof" >> liftM Typeof parseExpr)
        where
        parseProductType = reserved "struct" >> liftM Product parseSTypeRepr
        parseSumType = reserved "union" >> liftM Sum parseSTypeRepr
        -- stype-repr ::= '{' <type> ',' <type> ( ',' <type> )* [','] '}'
        parseSTypeRepr = braces $ do
            x1    <- parseType
            x2    <- comma >> parseType
            xs    <- many (comma >> parseType)
            optional comma
            return   (x1:x2:xs)
    -- qualifier ::= [ 'restrict' | 'volatile' ]
    parseQualifier =  (reserved "restrict" >> return Restrict)
                  <|> (reserved "volatile" >> return Volatile)
                  <|> (reserved "const"    >> return Const)

parseNominalDefinition :: Parser (Definition, NTypeRepr)
-- nominal-def ::= 'struct' <define> '{' <field>* '}'
--              |  'union' <define> '{' <field>* '}'
--              |  'enum' <define> '{' <enum-entry>* '}'
--              |  'bitpack' <define> '{' <bit-field | padding | alignment>* '}'
parseNominalDefinition = parseStruct <|> parseUnion <|> parseEnum <|> parseBitpack
    where
    parseStruct = do
        reserved "struct"
        name  <- liftM Id definition
        fs    <- braces $ many parseField
        return $ (name, Struct fs)
    parseUnion = do
        reserved "union"
        name  <- liftM Id definition
        fs    <- braces $ many parseField
        return $ (name, Union fs)
    parseEnum = do
        reserved "enum"
        name  <- liftM Id definition
        es    <- braces $ many enumEntry
        return $ (name, Enum es)
        -- enum-entry ::= <define> [ '=' <integer> ]
        where
        enumEntry = do
            name <- liftM Id definition
            expr <- optionMaybe (oper "=" >> parseExpr)
            return  (name, expr)
    parseBitpack = parserZero --STUB
    -- field ::= <type> [ <define> ] ';'
    parseField = do
        ty   <- parseType
        name <- option Anonymous (liftM Id definition)
        semi
        return  (name, ty)
    -- bit-field ::= <define> ':' ( 's' | 'u' ) <int-expr> ';'
    bitField = parserZero --STUB
    -- padding ::= 'sizeof' <int-expr> ';'
    -- alignment ::= 'alignof' <int-expr> ';'
          

parseFileStorage :: Parser StorageClass
-- file-storage ::= [ 'shared' | 'register' <quoted-name> ]
parseFileStorage =  (reserved "shared"   >> return Shared)
                <|> (reserved "register" >> liftM GlobalRegister quotedName)
                <|> (return PerThread)

parseFunctionStorage :: Parser StorageClass
-- function-storage ::= [ 'static' | 'shared' ]
parseFunctionStorage =  (reserved "static" >> return PerThread)
                    <|> (reserved "shared" >> return Shared)
                    <|> (return Stack)


parseProcItem :: Parser ProcedureSyntax
-- proc-item ::= <statement> ';' | '{' <proc-item>+ '}'
parseProcItem = parseCompountStatement <|> parseStatement --parse statement must come last
    where
    parseCompountStatement = do
        body     <- braces $ many1 parseProcItem
        let body' = case body of { [x] -> x; xs -> PBlock xs}
        return      body'
    -- statement ::= <val-decl>
    --            |  <var-decl>
    --            |  <assignment>
    --            |  [ <definition> ':' ] <control>
    --            |  <structured-goto>
    --            |  <return>
    --            |  <conditional>
    --            |  'label' <definition>
    --            |  'goto' <identifier>
    --            |  'pass'
    --            |  <expr>
    parseStatement =  parseValDecl
                  <|> parseVarDecl
                  <|> parseAssignment
                  <|> (option Anonymous (liftM Id definition << colon) >>= parseControl)
                  <|> parseStructuredGoto
                  <|> parseReturn
                  <|> parseConditional
                  <|> (reserved "label" >> liftM (PLabel . Id) definition)
                  <|> (reserved "goto" >> liftM PGoto identifier)
                  <|> (reserved "pass" >> return PPass)
                  <|> liftM PExpr parseExpr -- must go at bottom
        where
        -- val-decl ::= 'val' <definition> [ ':' <type> ] '=' <expr>
        parseValDecl = do
            reserved "val"
            name  <- liftM Id definition
            ty    <- optionMaybe (colon >> parseType)
            oper     "="
            expr  <- parseExpr
            return $ PVal name ty expr
        -- var-decl ::= 'var' <function-storage> <definition> [ ':' <type> ] '=' <expr>
        --           |  ['var'] <function-storage> <definition> ':' <type> [ '=' <expr> ]
        parseVarDecl = try one <|> two
            where
            one = do
                reserved "var"
                sc    <- parseFunctionStorage
                name  <- liftM Id definition
                ty    <- optionMaybe (colon >> parseType)
                expr  <- liftM Just parseExpr
                return $ PVar name sc ty expr
            two = do
                optional $ reserved "var"
                sc               <- parseFunctionStorage
                name             <- liftM Id definition
                colon
                ty               <- liftM Just parseType
                expr             <- optionMaybe (oper "=" >> parseExpr)
                return            $ PVar name sc ty expr
        -- assignment ::= <lval> (',' <lval>)* [','] '=' <expr> (',' <expr>)* [',']
        --             |  <lval> <op-assign> <expr>
        parseAssignment = try multi <|> acc
            where
            multi = do
                lvs  <- parseLVal `sepEndBy1` comma
                oper "="
                es   <- parseExpr `sepEndBy1` comma
                return $ PAssign lvs es
            acc = do
                    lval <- parseLVal
                    op   <- parseOpAssign
                    expr <- parseExpr
                    return $ PAccum lval op expr
                where
                parseOpAssign =  (oper "+="   >> return Add)
                             <|> (oper "-="   >> return Sub)
                             <|> (oper "*="   >> return Mul)
                             <|> (oper "/="   >> return Quo)
                             <|> (oper "%="   >> return Rem)
                             <|> (oper "//="  >> return Div)
                             <|> (oper "^^="  >> return Pow)
                             <|> (oper "^="   >> return Xor)
                             <|> (oper "&="   >> return And)
                             <|> (oper "|="   >> return Or)
                             <|> (oper "<<="  >> return Shl)
                             <|> (oper ">>="  >> return Shr)
                             <|> (oper "<<<=" >> return Rot)
        -- control ::= <switch>
        --          |  <if>
        --          |  'loop' <proc-item>
        --          |  <for-range> | <for-each>
        --          |  <exitwhen>
        parseControl name =  parseSwitch
                         <|> parseIf
                         <|> (reserved "loop" >> liftM (PLoop name) (inLoop parseProcItem))
                         <|> parseForRange
                         <|> parseForEach
                         <|> parseExitwhen
            where
            -- switch ::= 'switch' <expr> '{' <switch-case>* [ 'else' ':' <proc-item>+ ] '}'
            parseSwitch = inBranch $ do
                reserved "switch"
                expr  <- parseExpr
                cs    <- braces $ liftM2 (++) (many parseCase) parseDefault
                return $ PSwitch name expr cs
                -- switch-case ::= 'case' <expr> ':' <proc-item>+
                where
                parseCase = do
                    reserved    "case"
                    expr     <- parseExpr
                    colon
                    body     <- many1 parseProcItem
                    let body' = case body of { [x] -> x; x -> PBlock x }
                    return      (Just expr, body')
                parseDefault = option [] $ do
                    reserved    "else"
                    colon
                    body     <- many1 parseProcItem
                    let body' = case body of { [x] -> x; x -> PBlock x }
                    return      [(Nothing, body')]
            -- if ::= 'if' <expr> <proc-item> ( 'elif' <expr> <proc-item> )* 'else' <proc-item>
            parseIf = inBranch $ do
                reserved "if"
                c0    <-                          liftM2 (,) parseExpr parseProcItem
                cs    <- many (reserved "elif" >> liftM2 (,) parseExpr parseProcItem)
                reserved "else"
                f     <- parseProcItem
                return $ PIf name (c0:cs) f
            -- for-range ::= 'for' <definition> 'in' <expr> <direction> <expr> <proc-item>
            parseForRange = inLoop $ do
                reserved "for"
                ename <- liftM Id definition
                reserved "in"
                start <- parseExpr
                dir   <- parseDirection
                end   <- parseExpr
                body  <- parseProcItem
                return $ PForRange name ename start dir end body
                where
                -- direction ::= 'upto' | 'upthru' | 'downto' | 'downthru' 
                parseDirection =  (reserved "upto"     >> return Upto)
                              <|> (reserved "upthru"   >> return Upthru)
                              <|> (reserved "downto"   >> return Downto)
                              <|> (reserved "downthru" >> return Downthru)
            -- for-each ::= 'for' [ <definition> ',' ] <definition> 'in' <expr> <proc-item>
            parseForEach = inLoop $ do
                reserved "for"
                nname <- optionMaybe (liftM Id definition << comma)
                ename <- liftM Id definition
                reserved "in"
                expr  <- parseExpr
                body  <- parseProcItem
                return $ PForEach name (nname, ename) expr body
            -- exitwhen ::= 'exit' <proc-item> 'when' '{' <exit-case>* '}'
            parseExitwhen = inExitWhen $ do
                reserved "exit"
                body  <- parseProcItem
                reserved "when"
                outs  <- braces $ many parseCase
                return $ PExitWhen body outs
                where
                -- exit-case ::= 'case' <definition> ':' <proc-item>+
                parseCase = do
                    reserved    "case"
                    name     <- liftM Id definition
                    colon
                    body     <- many1 parseProcItem
                    let body' = case body of { [x] -> x; x -> PBlock x }
                    return      (name, body')
        -- structured-goto ::= ('fallthru' | 'break' | 'continue') [<identifier>]
        --                  |  [<identifier>] ('while' | 'until') <expr>
        --                  |  'exit' <identifier>
        parseStructuredGoto =  isInBranch (reserved "fallthru" >> liftM PFallthru (optionMaybe identifier))
                           <|> isInLoop (reserved "break" >> liftM PBreak (optionMaybe identifier))
                           <|> isInLoop (reserved "continue" >> liftM PContinue (optionMaybe identifier))
                           <|> isInLoop (optionMaybe identifier >>= (\name -> 
                                                            (reserved "while" >> liftM (flip PWhile name) parseExpr)
                                                        <|> (reserved "until" >> liftM (flip PUntil name) parseExpr)))
                           <|> isInExitWhen (reserved "exit" >> liftM PExit identifier)
        -- return ::= 'return' [ <expr> ]
        --         |  'jump' <expr>
        --         |  'yield' <expr>
        parseReturn =  isInProc InFunc (reserved "return" >> liftM PReturn (optionMaybe parseExpr))
                   <|> isInProc InFunc (reserved "jump" >> liftM PTailCall parseExpr)
                   <|> isInProc InIter (reserved "return" >> return (PReturn Nothing))
                   <|> isInProc InIter (reserved "yield" >> liftM PYield parseExpr)
        -- conditional ::= ( 'when' | 'unless' ) <expr> <proc-item>
        parseConditional = do
            which <- (reserved "when" >> return PWhen) <|> (reserved "unless" >> return PUnless)
            expr  <- parseExpr
            body  <- parseProcItem
            return $ which expr body


parseExpr :: Parser Expr
-- expr ::= <expr-term>
--       |  <funky-expr>
--       |  <expr-prefix> <expr>
--       |  <expr> <expr-infix> <expr>
--       |  <expr> ('to' | 'as' | ':') <type>
-- expr-prefix ::= '~' | '!' | '-'
-- expr-infix ::= '^^'
--             | '*' | '/' | '%' | '//'
--             | '+' | '-'
--             | '<<' | '>>' | '<<<'
--             | '&' | '|' | '^'
--             | '<' | '<=' | '==' | '!=' | '<>' | '>=' | '>'
--             | '&&' | '||'
parseExpr = buildExpressionParser exprOperators parseExprTerm
    where
    exprOperators = [ [ binary "^^" (Binary Pow) AssocRight ]
                    -- prefixes
                    , [ prefix "~" (Unary Invert)
                      , prefix "!" (Unary Not)
                      , prefix "-" (Unary Negate)
                      ]
                    -- arithmetic
                    , [ binary "*"  (Binary Mul) AssocLeft
                      , binary "/"  (Binary Quo) AssocLeft
                      , binary "%"  (Binary Rem) AssocLeft
                      , binary "//" (Binary Div) AssocLeft
                      ]
                    , [ binary "+" (Binary Add) AssocLeft
                      , binary "-" (Binary Sub) AssocLeft
                      ]
                    , [ binary "<<"  (Binary Shl) AssocLeft
                      , binary ">>"  (Binary Shr) AssocLeft
                      , binary "<<<" (Binary Rot) AssocLeft
                      ]
                    -- bitwise
                    , [ binary "&" (Binary And) AssocLeft
                      , binary "|" (Binary Or)  AssocLeft
                      , binary "^" (Binary Xor) AssocLeft
                      ]
                    -- relational
                    , [ binary "<"  (Binary Lt)   AssocNone
                      , binary "<=" (Binary Lte)  AssocNone
                      , binary "==" (Binary Eq)   AssocNone
                      , binary "!=" (Binary Neq)  AssocNone
                      , binary "<>" (Binary Ltgt) AssocNone
                      , binary ">=" (Binary Gte)  AssocNone
                      , binary ">"  (Binary Gt)   AssocNone
                      ]
                    -- logical
                    , [ binary "&&" (Binary AndAlso) AssocLeft
                      , binary "||" (Binary OrElse)  AssocLeft
                      ]
                    -- type casting and annotation
                    , [ Postfix (reserved "to" >> liftM CastTo parseType)
                      , Postfix (reserved "as" >> liftM CastAs parseType)
                      , Postfix (colon         >> liftM TypeAnn parseType)
                      ]
                    ]
    -- expr-term ::= <literal>
    --            |  <identifier>
    --            |  'vararg' <type>
    --            |  <constructor>
    --            |  'if' <expr> 'then' <expr> 'else' <expr>
    --            |  'do' '{' <proc-item>* '}'
    parseExprTerm =  liftM Lit parseLiteral
                 <|> liftM Var identifier
                 <|> liftM Vararg (reserved "vararg" >> parseType)
                 <|> parseCtor
                 <|> parseIfExpr
                 <|> parseDoExpr
                 <|> parseFunkyExpr
        where
        -- constructor ::= <arr-ctor> | <dynarr-ctor> | <prod-ctor> | <sum-ctor> | <nominal-ctor>
        parseCtor = parseArrCtor <|> parseDynArrCtor <|> parseProdCtor <|> parseSumCtor <|> parseNominalCtor
            where
            -- arr-ctor ::= '[' [<int-expr>] ']' '{' [ <array-elem> (',' <array-elem>)* ] [','] '}'
            parseArrCtor = do
                len   <- brackets $ optionMaybe parseExpr
                es    <- braces $ parseArrayElem `sepEndBy` comma
                return $ ArrExpr len es
            -- dynarr-ctor ::= '{' [ <array-elem> (',' <array-elem>)* ] [','] '}'
            parseDynArrCtor = liftM DynArrExpr (braces $ parseArrayElem `sepEndBy` comma)
            -- array-elem ::= [ <integer> '=' ] <expr>
            parseArrayElem = do
                i <-   optionMaybe $ try $ integer << reserved "="
                x <-   parseExpr
                return (i, x)
            -- prod-ctor ::= 'struct' '{' <expr> ',' <expr> ( ',' <expr> )* [','] '}'
            parseProdCtor = do
                reserved "struct"
                xs    <- braces $ parseExpr `sepEndBy` comma
                return $ ProdExpr xs
            -- sum-ctor ::= 'union' '{' <expr> ',' <expr> ( ',' <expr> )* [','] '}'
            parseSumCtor = do
                reserved "union"
                xs    <- braces $ parseExpr `sepEndBy` comma
                return $ SumExpr xs
            -- nominal-ctor ::= <identifier> '{' <struct-elem> (',' <struct-elem>)* [','] '}'
            parseNominalCtor = do
                    name  <- identifier
                    xs    <- braces $ parseStructElem `sepEndBy` comma
                    return $ NTypeExpr name xs
                where
                -- struct-elem ::= [ <identifier> '=' ] <expr>
                parseStructElem = do
                    name <- try $ optionMaybe (identifier << reserved "=")
                    e    <- parseExpr
                    return  (name, e)
        parseIfExpr = do
            reserved "if"
            p     <- parseExpr
            reserved "then"
            t     <- parseExpr
            reserved "else"
            f     <- parseExpr
            return $ IfExpr p t f
        parseDoExpr = do
            reserved "do"
            ss    <- braces $ many1 parseProcItem
            return $ DoExpr ss
    -- funky-expr ::= <expr> '(' <args> [ '...' <args> ] ')'
    --             |  <expr> '[' <expr> ']'
    --             |  <expr> '.' <member>
    --             |  <expr> '->' <member>
    --             |  '&' <lval>
    --             |  '*' <expr>
    --             |  'sizeof' <type>
    --             |  'alignof' <type>
    parseFunkyExpr =  parseCall
                  <|> parseIndex
                  <|> parseDot
                  <|> parseArrow
                  <|> (oper "&" >> liftM Address parseLVal)
                  <|> (oper "*" >> liftM Contents parseExpr)
                  <|> (reserved "sizeof" >> liftM Sizeof parseType)
                  <|> (reserved "alignof" >> liftM Alignof parseType)
        where
        parseCall = try $ do
                f      <- parseExpr
                (a, v) <- parens $ do
                            args  <- parseExpr `sepEndBy` comma
                            vargs <- optionMaybe (ellipsis >> parseExpr `sepEndBy` comma)
                            return   (args, vargs)
                return  $ Call f a v
        parseIndex = try $ do
            base  <- parseExpr
            ix    <- brackets $ parseExpr
            return $ Index base ix
        parseDot = try $ do
            obj <- parseExpr
            dot
            mem <- parseMember
            return $ Member obj mem
        parseArrow = try $ do
            obj <- parseExpr
            oper "->"
            mem <- parseMember
            return $ Member obj mem

parseLVal :: Parser LVal
-- lval ::= <lval> '[' <expr> ']' | <lval> '.' <member> | <lval> '->' <member>
--       |  '*' <lval>
--       |  <identifier>
parseLVal = buildExpressionParser lvalOps lvalTerm
    where
    lvalTerm = liftM LName identifier
    lvalOps = [ [ Postfix (     dot  >> liftM (flip LDot)     parseMember)
                , Postfix (oper "->" >> liftM (flip LArrow)   parseMember)
                , Postfix (             liftM (flip LIndex) $ braces parseExpr)
                ]
              , [ prefix "*" LStar ]
              ]


parseLiteral :: Parser Literal
parseLiteral =  liftM LitBool boolean
            <|> liftM LitInt integer
            <|> liftM LitFloat float
            <|> liftM LitChar charLit
            <|> liftM LitString stringLit

parseMember :: Parser (Either Integer String)
-- member ::= <identifer> | <integer>
parseMember = liftM Left integer <|> liftM Right identifier

------ Parsing Monad ------

type Parser = ParsecT String ParserState Identity

data ParserState = ParserState { _loopDepth   :: LoopDepth
                               , _branchDepth :: BranchDepth
                               , _exitDepth   :: ExitDepth
                               , _procType    :: ProcType
                               }
startState :: ParserState
startState = ParserState { _loopDepth   = 0
                         , _branchDepth = 0
                         , _exitDepth   = 0
                         , _procType    = InFile
                         }
type LoopDepth   = Integer
type BranchDepth = Integer
type ExitDepth   = Integer
type EnumControl = (Integer, [Integer])
data ProcType    = InFile | InFunc | InIter
    deriving Eq

inLoop       :: Parser a -> Parser a
inLoop        = _inX (\s -> s { _loopDepth   = _loopDepth s + 1 })
isInLoop      = _isInX (\s -> _loopDepth s > 0)
inBranch     :: Parser a -> Parser a
inBranch      = _inX (\s -> s { _branchDepth = _branchDepth s + 1 })
isInBranch    = _isInX (\s -> _branchDepth s > 0)
inExitWhen   :: Parser a -> Parser a
inExitWhen    = _inX (\s -> s { _exitDepth   = _exitDepth s + 1 })
isInExitWhen  = _isInX (\s -> _exitDepth s > 0)
inProc       :: ProcType -> Parser a -> Parser a
inProc ty     = _inX (\s -> s { _procType    = ty })
isInProc ty   = _isInX (\s -> _procType s == ty)

_inX :: (ParserState -> ParserState) -> Parser a -> Parser a
_inX f p = do
    s <- getState
    let s' = f s
    putState (s' `seq` s')
    result <- p
    putState s
    return result
_isInX :: (ParserState -> Bool) -> Parser a -> Parser a
_isInX f p = do
    cond <- liftM f getState
    if cond then p else parserZero

------ Easy Reading ------

identifier = Lex.identifier lexer
definition = Lex.lexeme lexer . try $ _raw_id >>= _isRenamable
reserved   = Lex.reserved   lexer
quotedName = (Lex.lexeme lexer . try $ str >>= _isRenamable) <?> "quoted name"
    where str = between (char '"') (char '"' <?> "end of quoted name") $ many (_character "\"" <?> "quoted name character")
oper       = Lex.reservedOp lexer

parens     = Lex.parens     lexer
brackets   = Lex.brackets   lexer
braces     = Lex.braces     lexer

boolean    = do
    x <- identifier
    case x of
        "true"  -> return True
        "false" -> return False
        _       -> parserZero
integer    = Lex.integer    lexer
float      = Lex.float      lexer
charLit    = Lex.lexeme lexer (quotes aChar) <?> "character"
    where
    quotes = between (char '\'') (char '\'' <?> "end of character")
    aChar  = _character "'" <?> "literal character"
stringLit  = liftM concat $ many1 (Lex.lexeme lexer (quotes aString <?> "literal string"))
    where
    quotes  = between (char '"') (char '"' <?> "end of string")
    aString = many (_character "\"" <?> "string character")

colon      = Lex.colon      lexer
semi       = Lex.semi       lexer
dot        = Lex.dot        lexer
comma      = Lex.comma      lexer
ellipsis   = Lex.symbol     lexer "..."

pathName = Lex.lexeme lexer (relPath <|> sysPath) <?> "path"
    where
    relPath = quote (many pathChar)
        where quote = between (char '"') (char '"' <?> "end of path")
    sysPath = quote (many pathChar)
        where quote = between (char '<') (char '>' <?> "end of path")
    pathChar = alphaNum <|> oneOf "_/." <?> "path character"


_raw_id :: Parser String
_raw_id = (liftM2 (:) (identStart nautilusDef) (many $ identLetter nautilusDef)) <?> "identifier"
_isRenamable :: String -> Parser String
_isRenamable x = if x `elem` nautilusNotRenamable
                    then unexpected $ "reserved word " ++ show x
                    else return x


_character :: [Char] -> Parser Char
_character forbid =  satisfy normal
              <|> try (char '\\' >>
                        (   special
                        --TODO? '\^C' for ctrl+C, &c
                        <|> ascii8
                        <|> ascii16
                        <|> uni4
                        <|> uni6   ))
    where
    normal c =  (c >= ' ')
             && (c `notElem` "\DEL\\")
             && (c `notElem` forbid)
    special = do
            p <- oneOf (map fst table)
            return $ fromJust $ lookup p table
        where table = [ ('0' , '\0')
                      , ('a' , '\a')
                      , ('b' , '\b')
                      , ('e' , '\27')
                      , ('f' , '\f')
                      , ('n' , '\n')
                      , ('r' , '\r')
                      , ('t' , '\t')
                      , ('\'', '\'')
                      , ('\"', '\"')
                      , ('\\', '\\')
                      ]
    ascii8 = liftM (chr . fromInteger . _number 8) (oneOf "o" >> count 3 octDigit)
    ascii16 = liftM (chr . fromInteger . _number 16) (oneOf "x" >> count 2 hexDigit)
    uni4 = liftM (chr . fromInteger . _number 16) (char 'u' >> count 4 hexDigit)
    uni6 = char 'U' >> (high <|> low)
        where
        low  = liftM (chr .                fromInteger . _number 16) (char '0' >> (count 5 hexDigit))
        high = liftM (chr . (+ 0x100000) . fromInteger . _number 16) (string "10" >> (count 4 hexDigit))

_number :: Integer -> String -> Integer
_number base digits = let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
                      in n `seq` n

a << b = do { x <- a; b; return x }
binary  n f = Infix   (oper n >> return f)
prefix  n f = Prefix  (oper n >> return f)
postfix n f = Postfix (oper n >> return f)
nada = return ()




