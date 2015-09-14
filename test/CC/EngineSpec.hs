{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CC.EngineSpec
    ( main
    , spec
    ) where

import CC.Engine
import CC.Result

import Test.Hspec

import Data.List (sort)
import Data.Text (Text)
import System.Directory
    ( createDirectoryIfMissing
    , getCurrentDirectory
    , setCurrentDirectory
    )
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "analyzeFiles" $ do
        it "can analyze files" $ do
            (IssueResult Issue{..}:_) <- withinTempDir $ do
                createFile "example.hs" $ T.unlines
                    [ "main :: IO ()"
                    , "main = do"
                    , "    print \"redundant do\""
                    ]

                analyzeFiles ["example.hs"]

            issueDescription `shouldBe` "Redundant do"

            let (Location path (Position line column) _) = issueLocation

            path `shouldBe` "example.hs"
            (line, column) `shouldBe` (2, 8)

    describe "hsFiles" $ do
        it "find Haskell files given a list of sources" $ do
            paths <- withinTempDir $ do
                createFile "foo.hs" ""
                createFile "bar.hs" ""
                createFile "foo/bar.hs" ""
                createFile "foo/bar/baz.hs" ""
                createFile "foo/bar/bat.hs" ""
                createFile "foo/baz/bar.hs" ""
                createFile "foo/baz/bat.hs" ""

                hsFiles ["foo.rb", "bar.hs", "foo/bar/"]

            sort paths `shouldBe`
                [ "bar.hs"
                , "foo/bar/bat.hs"
                , "foo/bar/baz.hs"
                ]

withinTempDir :: IO a -> IO a
withinTempDir act = withSystemTempDirectory "cc-hlint" $ \tmp -> do
    E.bracket getCurrentDirectory setCurrentDirectory $ \_ ->
        setCurrentDirectory tmp >> act

createFile :: FilePath -> Text -> IO ()
createFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    T.writeFile path content
