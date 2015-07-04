module CC.Config
    ( Config(..)
    , loadConfig
    ) where

data Config = Config
    { configExcludes :: [FilePath]
    }

loadConfig :: FilePath -> IO Config
loadConfig _ = return $ Config []
