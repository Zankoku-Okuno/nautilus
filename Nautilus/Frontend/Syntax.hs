module Nautilus.Frontend.Syntax where

-- Names are uses of members, enum values, and break&c targets
newtype Name = Name String
-- Identifiers are uses of types, values, variables, and functions, and can descend into enums
data Identifier = Id String | QualId [String]
-- Definitions define types, values, variables, functions, and break&c targets
data Definition = Define String | Anonymous
instance Eq Definition where
    Define a == Define b = a == b
    _ == _ = False


data FileSyntax = FImport   Visibility String ImportRestrict
                | FNewType  Visibility Definition NTypeRepr
                | FTypeSyn  Visibility Definition Type
                | FOpen     Visibility Identifier (Maybe [Name])
                | FVal      Visibility Definition (Maybe Type) Expr
                | FVar      Visibility Definition StorageClass (Maybe Type) (Maybe Expr)
                | FVarDecl  Visibility Definition (Maybe String) Type
                | FFuncDecl Visibility [Attribute] Definition (Maybe String) Type
                | FTypeDecl Visibility Definition
                | FFunction Visibility [Attribute] Definition (Maybe String) Type ([(Definition, Type)], Type) ProcedureSyntax
                --TODO remember that inline functions can't rely on intern'd globals or functions, unless they are themselves intern
                | FModule   Visibility Definition [Definition] [FileSyntax]
                | FInstance Visibility Definition Identifier [Type]
                | FLocal    [FileSyntax] [FileSyntax]
data ImportRestrict = All | ImportAs Definition | Open [Name] | Hide [Name]

data Visibility = Intern | Opaque | Extern
    deriving (Eq)
data Attribute = Inline | Iterator | NoReturn | Pure | VarArgs
    deriving (Eq)


data Type = Void
          | Nominal    Identifier
          | Structural STypeRepr
          | Pointer    Type
          | Reference  Type
          | Array      Expr Type
          | Function   [Type] Type
          | Any
          | Typeof     Expr
          | QualType   Qualifier Type

data NTypeRepr = Primitive PrimRepr
               | Struct    [(Definition, Type)]
               | Union     [(Definition, Type)]
               | Enum      [(Definition, Maybe Expr)]
--               | BitPack   [(Definition, (Integer, Sign))]
data STypeRepr = Product [Type]
               | Sum     [Type]
data PrimRepr = PrimBool
              | PrimSize  | PrimDiff
              | PrimByte  | PrimAscii
              | PrimShort | PrimUShort
              | PrimInt   | PrimUInt
              | PrimLong  | PrimULong
              | PrimHalfFloat
              | PrimFloat


data Qualifier = Volatile | Distinct | Const
    deriving (Eq)

data StorageClass = Stack | Parameter | PerThread | Shared | GlobalRegister String
    deriving (Eq)

--TODO? effect system


data ProcedureSyntax = PVal      Definition (Maybe Type) Expr
                     | PVar      Definition StorageClass (Maybe Type) (Maybe Expr)
                     | PExpr     Expr
                     | PAssign   [LVal] [Expr]
                     | PAccum    LVal BinaryOp Expr
                     | PBlock    [ProcedureSyntax]
                     | PSwitch   Definition Expr [(Maybe Expr, ProcedureSyntax)]
                     | PIf       Definition [(Expr, ProcedureSyntax)] ProcedureSyntax
                     | PFallthru (Maybe Name)
                     | PWhen     Expr ProcedureSyntax
                     | PUnless   Expr ProcedureSyntax
                     | PLoop     Definition ProcedureSyntax
                     | PWhile    Expr (Maybe Name)
                     | PUntil    Expr (Maybe Name)
                     | PForRange Definition Definition Expr Direction Expr ProcedureSyntax
                     | PForEach  Definition (Maybe Definition, Definition) Expr ProcedureSyntax
                     | PBreak    (Maybe Name)
                     | PContinue (Maybe Name)
                     | PExitWhen ProcedureSyntax [(Definition, ProcedureSyntax)]
                     | PExit     Name
                     | PReturn   (Maybe Expr)
                     | PYield    Expr
                     | PTailCall Expr
                     | PLabel    Definition
                     | PGoto     Name
                     | PPass
data Direction = Upto | Upthru | Downto | Downthru


data Expr = Lit        Literal
          | Var        Identifier
          | Vararg     Type
          | ArrExpr    (Maybe Expr) [(Maybe Integer, Expr)]
          | DynArrExpr [(Maybe Integer, Expr)]
          | ProdExpr   [Expr]
          | SumExpr    Integer Expr
          | NTypeExpr  Identifier [(Maybe Name, Expr)]
          | Address    LVal
          | Contents   Expr
          | Index      Expr Expr
          | Member     Expr (Either Integer Name)
          | Arrow      Expr (Either Integer Name)
          | Unary      UnaryOp   Expr
          | Binary     BinaryOp  Expr Expr
          | IfExpr               Expr Expr Expr
          | DoExpr               [ProcedureSyntax]
          | Call       Expr      [Expr] (Maybe [Expr])
          | CastTo     Type      Expr
          | CastAs     Type      Expr
          | TypeAnn    Type      Expr
          | Sizeof     Type
          | Alignof    Type

data LVal = LName       Identifier
          | LStar  LVal
          | LIndex LVal Expr
          | LDot   LVal (Either Integer Name)
          | LArrow LVal (Either Integer Name)

data Literal = LitBool   Bool
             | LitInt    Integer
             | LitFloat  Double
             | LitChar   Char
             | LitString String


data UnaryOp = Negate | Invert | Not
data BinaryOp = Add | Sub | Mul | Quo | Rem | Div | Pow
              | Shl | Shr | Rot
              | And | Or  | Xor
              | Eq  | Neq | Gt  | Lt  | Gte | Lte | Ltgt
              | AndAlso | OrElse




