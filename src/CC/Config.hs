{-# LANGUAGE OverloadedStrings #-}
module CC.Config
    ( Config(..)
    , loadConfig
    ) where

import Data.Aeson (FromJSON(..), (.:), decode, withObject)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Directory (doesFileExist)

import qualified Data.Text.Lazy.IO as T

data Config = Config { configExcludes :: [FilePath] }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        Config <$> o .: "exclude_paths"

loadConfig :: FilePath -> IO Config
loadConfig fp = do
    exists <- doesFileExist fp
    mconfig <- if exists
        then decodeText <$> T.readFile fp
        else return Nothing

    return $ fromMaybe (Config []) mconfig

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . encodeUtf8
