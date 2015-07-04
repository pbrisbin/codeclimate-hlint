module CC.Analyze
    ( analyzeFiles
    ) where

import CC.Result

import Language.Haskell.HLint3
    ( applyHints
    , autoSettings
    , parseModuleEx
    )

analyzeFiles :: [FilePath] -> IO [Result]
analyzeFiles = fmap concat . mapM analyzeFile

analyzeFile :: FilePath -> IO [Result]
analyzeFile fp = do
    (flags, classify, hint) <- autoSettings

    either
        (\e -> [ModuleFailure e])
        (\m -> map Issue $ applyHints classify hint [m])
        <$> parseModuleEx flags fp Nothing
