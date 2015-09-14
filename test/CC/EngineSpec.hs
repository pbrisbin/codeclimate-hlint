{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CC.EngineSpec
    ( main
    , spec
    ) where

import CC.Engine
import CC.Result

import Test.Hspec

import System.IO.Temp

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "analyzeFiles" $ do
        it "can analyze files" $ do
            (IssueResult Issue{..}:_) <- withTempDir $ \_ -> do
                T.writeFile "example.hs" $ T.unlines
                    [ "main :: IO ()"
                    , "main = do"
                    , "    print \"redundant do\""
                    ]

                analyzeFiles ["example.hs"]

            issueDescription `shouldBe` "Redundant do"

            let (Location path (Position line column) _) = issueLocation

            path `shouldBe` "example.hs"
            (line, column) `shouldBe` (2, 8)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "cc-hlint"
