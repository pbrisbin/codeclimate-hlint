{-# LANGUAGE OverloadedStrings #-}
module Main where

import CC.Analyze
import CC.Config
import CC.Result

import Data.Aeson (encode)
import Data.Monoid ((<>))
import System.FilePath.Glob (compile, globDir)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    config <- loadConfig "/config.json"
    let included = not . (`elem` configExcludes config)

    mapM_ printResult
        =<< analyzeFiles
        =<< filter included <$> hsFiles

hsFiles :: IO [FilePath]
hsFiles = concat . fst <$> globDir [compile "**/*.hs"] "."

printResult :: Result -> IO ()
printResult = BL.putStr . (<> "\0") . encode
