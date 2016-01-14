module CC.Engine
    ( analyzeFiles
    , hsFiles
    ) where

import CC.Result

import Data.List (isSuffixOf, partition)
import Language.Haskell.HLint3
    ( applyHints
    , Classify
    , Hint
    , ParseFlags
    , defaultParseFlags
    , findSettings
    , parseFlagsAddFixities
    , parseModuleEx
    , readSettingsFile
    , resolveHints
    )
import System.Environment (lookupEnv)
import System.FilePath.Glob (compile, globDir)

analyzeFiles :: [FilePath] -> IO [Result]
analyzeFiles = fmap concat . mapM analyzeFile

analyzeFile :: FilePath -> IO [Result]
analyzeFile fp = do
    (flags, classify, hint) <- hlintSettings

    either
        -- Returning the error result materializes as a type:warning output not
        -- yet supported by Platform. CLI ignores it, but codeclimate.com
        -- errors. For now, we discard the parse failures here.
        --(\e -> [ErrorResult e])
        (const [])
        (\m -> map resultFromIdea $ applyHints classify hint [m])
        <$> parseModuleEx flags fp Nothing

-- Custom version of @'autoSettings'@ that overrides the HLint data directory
-- from @HLINT_DATA_DIR@, if found in @ENV@.
hlintSettings :: IO (ParseFlags, [Classify], Hint)
hlintSettings = do
    mdir <- lookupEnv "HLINT_DATA_DIR"
    (fixities, classify, hints) <- findSettings (readSettingsFile mdir) Nothing
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, resolveHints hints)

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
