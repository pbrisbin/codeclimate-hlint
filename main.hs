{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Aeson hiding (Result)
import Data.Char (toLower, toUpper)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.FilePath.Glob
import Language.Haskell.HLint3
import Language.Haskell.Exts.SrcLoc

import qualified Data.Map as M
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
        , "check" .= check
        , "description" .= format ideaFrom ideaTo
        , "categories" .= [category]
        , "remediation_points" .= (1000000 :: Int)
        , "location" .= ideaSpan
        ]
      where
        format from Nothing = "Found " ++ show from ++ ", remove it"
        format from (Just to) = "Found " ++ show from ++ ", why not " ++ show to

        check = "HLint/" <> camelize ideaHint
        category = fromMaybe "Style" $ M.lookup ideaHint categories

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

-- As we find hints that are non-style we can add entries here. For now it's
-- empty meaning all will get the default value, "Style"
categories :: M.Map String Text
categories = M.empty

camelize :: String -> String
camelize = concatMap capitalize . words

capitalize :: String -> String
capitalize [] = []
capitalize (c:rest) = toUpper c : map toLower rest
