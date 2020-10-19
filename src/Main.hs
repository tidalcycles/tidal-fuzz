import Sound.Tidal.Types

main = do aha <- Sound.Tidal.Types.wWalk $ Sig [] $ Pattern Osc
          putStrLn $ show aha
