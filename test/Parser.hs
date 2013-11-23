import System.Exit
import Nautilus.Frontend.Parser

testFiles = map ("test/parser/"++)
            [ "comments.n"
            , "import.n"
            , "types.n"
            , "struct.n"
            , "open.n"
            , "val_var.n"
            , "decl.n"
            , "func.n"
            , "module.n"
            , "visibility.n"
            , "local.n"
            --TODO statements
            --TODO expressions
            ]

main = do
	mapM testCase testFiles
	putStrLn "OK"
	exitSuccess


testCase :: FilePath -> IO ()
testCase file = do
	putStrLn $ "   " ++ file
	text <- readFile file
	case runNautilusParser file text of
		Left err -> putStrLn "ERROR" >> print err >> exitFailure
		_ -> return ()
