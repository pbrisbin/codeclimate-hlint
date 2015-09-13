module Main where

import CC.Config
import CC.Engine
import CC.Result

main :: IO ()
main = do
    sources <- configIncludes <$> loadConfig "/config.json"

    mapM_ printResult =<< analyzeFiles =<< hsFiles sources

