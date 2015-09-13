{-# LANGUAGE OverloadedStrings #-}
module Main where

import CC.Analyze
import CC.Config
import CC.Result

import Data.Aeson (encode)
import Data.List (isSuffixOf, partition)
import Data.Monoid ((<>))
import System.FilePath.Glob (compile, globDir)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    sources <- configIncludes <$> loadConfig "/config.json"

    mapM_ printResult =<< analyzeFiles =<< hsFiles sources

hsFiles :: [FilePath] -> IO [FilePath]
hsFiles sources = do
    let (dirs, files) = partition isDirectory sources
        patterns = map (compile . (++ "**/*.hs")) dirs

    results <- concat . fst <$> globDir patterns "."

    return $ map clean $ files ++ results

  where
    isDirectory = ("/" `isSuffixOf`)

    -- TODO: I know where one ./ comes from, but not the other...
    clean ('.':'/':'.':'/':x) = x
    clean x = x

printResult :: Result -> IO ()
printResult = BL.putStr . (<> "\0") . encode
