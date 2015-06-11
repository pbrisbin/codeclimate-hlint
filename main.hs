module Main where

import Data.Aeson hiding (Result)
import System.FilePath.Glob
import Language.Haskell.HLint3

import qualified Data.ByteString.Lazy as BL

data Result
    = Issue Idea
    | ModuleFailure ParseError

instance ToJSON Result where
    toJSON _ = undefined -- TODO (easy)

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
