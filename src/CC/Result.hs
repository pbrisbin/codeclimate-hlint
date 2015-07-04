{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CC.Result
    ( Result(..)
    ) where

import Data.Aeson hiding (Result)
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.HLint3

import qualified Data.Map as M

data Position = Position Int Int

instance ToJSON Position where
    toJSON (Position line column) = object
        [ "line" .= line
        , "column" .= column
        ]

data Location = Location FilePath Position Position

instance ToJSON Location where
    toJSON (Location path begin end) = object
        [ "path" .= path
        , "positions" .= object
            [ "begin" .= begin
            , "end" .= end
            ]
        ]

fromSrcSpan :: SrcSpan -> Location
fromSrcSpan SrcSpan{..} = Location
    srcSpanFilename
    (Position srcSpanStartLine srcSpanStartColumn)
    (Position srcSpanEndLine srcSpanEndColumn)

data Result = Issue Idea | ModuleFailure ParseError

instance ToJSON Result where
    toJSON (Issue Idea{..}) = object
        [ "type" .= ("issue" :: Text)
        , "check_name" .= check
        , "description" .= format ideaFrom ideaTo
        , "categories" .= [category]
        , "location" .= fromSrcSpan ideaSpan
        , "remediation_points" .= (100000 :: Int)
        ]
      where
        format from Nothing = "Found " ++ show from ++ ", remove it"
        format from (Just to) = "Found " ++ show from ++ ", why not " ++ show to

        check = "HLint/" <> camelize ideaHint
        category = fromMaybe "Style" $ M.lookup ideaHint categories

    toJSON (ModuleFailure _) = object
        [ "type" .= ("warning" :: Text)
        ]

-- As we find hints that are non-style we can add entries here. For now it's
-- empty meaning all will get the default value, "Style"
categories :: M.Map String Text
categories = M.empty

camelize :: String -> String
camelize = concatMap capitalize . words

capitalize :: String -> String
capitalize [] = []
capitalize (c:rest) = toUpper c : map toLower rest
