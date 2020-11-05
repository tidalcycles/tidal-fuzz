import Sound.Tidal.Types
import System.Environment

main = do
          args <- getEnvVars
          aha <- Sound.Tidal.Types.wWalk $ Sig [] $ Pattern Osc
          putStrLn $ show aha
