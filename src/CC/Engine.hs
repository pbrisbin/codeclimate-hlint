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
import System.FilePath.Glob (compile, globDir)

hlintDataDir :: Maybe FilePath
hlintDataDir = Just "/home/app/hlint-src"

-- This is basically copy-paste from HLint's autoSettings, but adjusted to look
-- in the correct place for HLint config
hlintSettings :: IO (ParseFlags, [Classify], Hint)
hlintSettings = do
    (fixities, classify, hints) <- findSettings (readSettingsFile hlintDataDir) Nothing
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, resolveHints hints)

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
