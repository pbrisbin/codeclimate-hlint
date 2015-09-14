{-# LANGUAGE OverloadedStrings #-}
module Main where

import CC.Config
import CC.Engine

import Data.Aeson (encode)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    sources <- configIncludes <$> loadConfig "/config.json"
    results <- analyzeFiles =<< hsFiles sources

    mapM_ (BL.putStr . (<> "\0") . encode) results
