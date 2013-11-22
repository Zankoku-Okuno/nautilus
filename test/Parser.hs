import System.Exit
import Nautilus.Frontend.Parser

testFiles = map ("test/parser/"++)
            [ "comments.n"
            , "import.n"
            , "types.n"
            , "struct.n"
            ]

main = do
	mapM testCase testFiles
	putStrLn "No errors"
	exitSuccess


testCase :: FilePath -> IO ()
testCase file = do
	text <- readFile file
	case runNautilusParser file text of
		Left err -> putStrLn "ERROR" >> print err >> exitFailure
		_ -> return ()
