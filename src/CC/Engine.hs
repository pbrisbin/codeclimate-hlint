module CC.Engine
    ( analyzeFiles
    , hsFiles
    ) where

import CC.Result

import Data.List (isSuffixOf, partition)
import Language.Haskell.HLint3
    ( applyHints
    , autoSettings
    , parseModuleEx
    )
import System.FilePath.Glob (compile, globDir)

analyzeFiles :: [FilePath] -> IO [Result]
analyzeFiles = fmap concat . mapM analyzeFile

analyzeFile :: FilePath -> IO [Result]
analyzeFile fp = do
    (flags, classify, hint) <- autoSettings

    either
        (\e -> [ModuleFailure e])
        (\m -> map Issue $ applyHints classify hint [m])
        <$> parseModuleEx flags fp Nothing

hsFiles :: [FilePath] -> IO [FilePath]
hsFiles sources = do
    let (dirs, files) = partition isDirectory sources
        patterns = map (compile . (++ "**/*.hs")) dirs

    results <- concat . fst <$> globDir patterns "."

    return $ map clean $ files ++ results

  where
    isDirectory = ("/" `isSuffixOf`)

    -- TODO: I know where one ./ comes from, but not the other...
    clean ('.':'/':'.':'/':x) = x
    clean x = x
