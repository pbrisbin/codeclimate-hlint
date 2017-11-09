{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CC.Result
    ( Result(..)
    , resultFromIdea

    -- Exported for testing
    , Issue(..)
    , Location(..)
    , Position(..)
    ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Haskell.Exts.SrcLoc (SrcSpan(..))
import Language.Haskell.HLint3 (Idea(..), ParseError, Severity(..))

import qualified Data.Text as T

data Position = Position Int Int deriving Show

instance ToJSON Position where
    toJSON (Position line column) = object
        [ "line" .= line
        , "column" .= column
        ]

data Location = Location FilePath Position Position deriving Show

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
    (locationFileName srcSpanFilename)
    (Position srcSpanStartLine srcSpanStartColumn)
    (Position srcSpanEndLine srcSpanEndColumn)
  where
    locationFileName ('.':'/':x) = x
    locationFileName x = x

data Result
    = IssueResult Issue
    | ErrorResult ParseError

instance Show Result where
    show (IssueResult issue) = show issue
    show (ErrorResult _) = "parse error"

data Issue = Issue
    { issueType :: Text
    , issueCheckName :: Text
    , issueDescription :: Text
    , issueContent :: Text -- TODO: Markdown type
    , issueCategories :: [Text]
    , issueLocation :: Location
    , issueRemediationPoints :: Int
    }
    deriving Show

resultFromIdea :: Idea -> Result
resultFromIdea Idea{..} = IssueResult Issue
    { issueType = "issue"
    , issueCheckName = "HLint/" <> T.pack (camelize ideaHint)
    , issueDescription = T.pack ideaHint
    , issueContent = content ideaFrom ideaTo
    , issueCategories = categories ideaHint
    , issueLocation = fromSrcSpan ideaSpan
    , issueRemediationPoints = points ideaSeverity
    }

  where
    content from Nothing = T.unlines
        [ "Found"
        , ""
        , "```"
        , T.pack from
        , "```"
        , ""
        , "remove it."
        ]

    content from (Just to) = T.unlines
        [ "Found"
        , ""
        , "```"
        , T.pack from
        , "```"
        , ""
        , "Why not"
        , ""
        , "```"
        , T.pack to
        , "```"
        ]

    categories _ = ["Style"]

    points Ignore = 0
    points Suggestion = 0
    points Warning = 100 * 1000
    points Error = 500 * 1000

instance ToJSON Result where
    toJSON (IssueResult Issue{..}) = object
        [ "type" .= issueType
        , "check_name" .= issueCheckName
        , "description" .= issueDescription
        , "content" .= object
            [ "body" .= issueContent
            ]
        , "categories" .= issueCategories
        , "location" .= issueLocation
        , "remediation_points" .= issueRemediationPoints
        ]

    toJSON (ErrorResult _) = object
        [ "type" .= ("warning" :: Text)
        ]

camelize :: String -> String
camelize = concatMap capitalize . words

capitalize :: String -> String
capitalize [] = []
capitalize (c:rest) = toUpper c : rest
