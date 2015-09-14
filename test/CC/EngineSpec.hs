{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CC.EngineSpec
    ( main
    , spec
    ) where

import CC.Engine
import CC.Result

import Test.Hspec

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "analyzeFiles" $ do
        it "can analyze files" $ do
            (IssueResult Issue{..}:_) <- withinTempDir $ do
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

withinTempDir :: IO a -> IO a
withinTempDir act = withSystemTempDirectory "cc-hlint" $ \tmp -> do
    -- TODO: exception safety
    dir <- getCurrentDirectory
    setCurrentDirectory tmp
    result <- act
    setCurrentDirectory dir
    return $ result
