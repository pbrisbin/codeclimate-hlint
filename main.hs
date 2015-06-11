{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Aeson hiding (Result)
import Data.Text (Text)
import System.FilePath.Glob
import Language.Haskell.HLint3
import Language.Haskell.Exts.SrcLoc

import qualified Data.ByteString.Lazy as BL

data Result
    = Issue Idea
    | ModuleFailure ParseError

instance ToJSON SrcSpan where
    toJSON SrcSpan{..} = object
        [ "path" .= srcSpanFilename
        , "begin" .= object
            [ "pos" .= srcSpanStartColumn
            , "line" .= srcSpanStartLine
            ]
        , "end" .= object
            [ "pos" .= srcSpanEndColumn
            , "line" .= srcSpanEndLine
            ]
        ]

instance ToJSON Result where
    toJSON (Issue Idea{..}) = object
        [ "type" .= ("issue" :: Text)
        , "check" .= ("HLint/x" :: Text) -- TODO
        , "description" .= ("hlint issue identified" :: Text)  -- TODO
        , "categories" .= ["Style" :: Text]
        , "remediation_points" .= (1000000 :: Int)
        , "location" .= ideaSpan
        ]

    toJSON (ModuleFailure _) = object
        [ "type" .= ("warning" :: Text)
        ]

main :: IO ()
main = mapM_ printResult . concat =<< mapM analyzeFile =<< getFiles

getFiles :: IO [FilePath]
getFiles = concat . fst <$> globDir [compile "**/*.hs"] "/code"

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
