module Sound.Tidal.Walk where

import Sound.Tidal.Types

import System.Random



walk :: IO String
walk = do let target = Sig [] (Pattern Osc)
          step target

step :: Sig -> IO String
step t = do r <- randomIO
            return $ pick r $ words "every sometimes jux"

pick :: Float -> [a] -> a
pick f xs = xs !! (floor $ f * (fromIntegral $ length xs))
