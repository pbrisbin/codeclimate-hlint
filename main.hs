module Main where

import CC.Analyze
import CC.Config
import CC.Result

import Data.Aeson (encode)
import System.FilePath.Glob (compile, globDir)

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

printResult :: Result -> IO ()
printResult result = do
    BL.putStr $ encode result
    putChar '\0'
