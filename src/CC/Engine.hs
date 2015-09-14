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
        (\e -> [ErrorResult e])
        (\m -> map resultFromIdea $ applyHints classify hint [m])
        <$> parseModuleEx flags fp Nothing

hsFiles :: [FilePath] -> IO [FilePath]
hsFiles sources = do
    let (dirs, files) = partition isDirectory sources
        patterns = map (compile . (++ "**/*.hs")) dirs

    results <- concat . fst <$> globDir patterns "."

    return $ map clean $ filter isHaskell files ++ results

  where
    isDirectory = ("/" `isSuffixOf`)
    isHaskell = (".hs" `isSuffixOf`)

    clean ('.':'/':x) = x
    clean x = x
