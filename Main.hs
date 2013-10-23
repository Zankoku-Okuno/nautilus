module Main where
import Nautilus.Frontend.Analyze

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

progVersion = "0.0.0.0"

main :: IO ()
main = do
    putStrLn "Mesdames, messieurs, bon soir."
    putStrLn "Goodbyte, cruel world!"
    exitWith ExitSuccess


-- The following architecture is taken from http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt

data Options = Options  { } deriving (Show)
startOptions = Options  { }

getOptions :: IO (Options, [String])
getOptions = do
    (actions, args, errors) <- return . getOpt Permute options =<< getArgs
    if errors == []
      then do
        opts <- foldl (>>=) (return startOptions) actions
        return (opts, args)
      else do
        mapM (hPutStrLn stderr) errors
        exitFailure

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr progVersion
                exitWith ExitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr $ "TODO(describe program)"
                hPutStrLn stderr $ "TODO(add license note)"
                hPutStrLn stderr $ usageInfo (prg ++ " TODO(usage)") options
                exitWith ExitSuccess))
        "Show help"
    ]

