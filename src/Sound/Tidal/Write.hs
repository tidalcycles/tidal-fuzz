module Sound.Tidal.Write where

import Sound.Tidal.TypesPrime
import Control.Monad

walkOut = do
              string <- wWalk $ Sig [] $ Pattern Osc
              return (show string ++ "\n\n")


writeText :: IO ()
writeText = do
              iterations <- getLine
              -- to do, turn into recursion with n iterations.
              o <- walkOut
              o' <- walkOut
              o'' <- walkOut
              writeFile "example.txt" (join [o, o', o''])
