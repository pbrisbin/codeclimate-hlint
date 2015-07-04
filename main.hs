module Main where

import CC.Config
import CC.Result

import Data.Aeson hiding (Result)
import System.FilePath.Glob
import Language.Haskell.HLint3

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    config <- loadConfig "/config.json"

    let included = not . (`elem` configExcludes config)

    files <- hsFiles
    results <- analyzeFiles $ filter included files

    mapM_ printResult results

hsFiles :: IO [FilePath]
hsFiles = concat . fst <$> globDir [compile "**/*.hs"] "."

analyzeFiles :: [FilePath] -> IO [Result]
analyzeFiles = fmap concat . mapM analyzeFile

analyzeFile :: FilePath -> IO [Result]
analyzeFile fp = do
    (flags, classify, hint) <- autoSettings

    either
        (\e -> [ModuleFailure e])
        (\m -> map Issue $ applyHints classify hint [m])
        <$> parseModuleEx flags fp Nothing

printResult :: Result -> IO ()
printResult result = do
    BL.putStr $ encode result
    putChar '\0'
