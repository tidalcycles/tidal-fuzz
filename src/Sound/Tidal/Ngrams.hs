module Sound.Tidal.Ngrams where

import System.IO
import Data.List
import Data.Ord
import Data.Char
import Data.String
import Data.Scientific
import Sound.Tidal.Tokeniser


-- lookup with tokeniser v2 ..
lookupT :: IO ([([String], Int)])
lookupT = do
              -- add path to directory..
              handle <- openFile "Sound/Tidal/tidal-input.txt" ReadMode
              contents <- hGetContents handle
              let reformat = removePunc $ contents
                              -- to do: remove # and $ ?
                  breakup = breakupCode "\n\n" reformat
                  tokenised = map (tokeniser) $ breakup
                  order = 2 -- change to above line to control ngram size..
                  -- turns input into a list of strings separated by " "
                  resplit = concat (intersperse [" "] tokenised)
                  -- get all possible ngrams
                  ngramFreqs = ngramSort $ ngram order resplit
              return ngramFreqs

--ngram out function
ngramOut :: [([String], Int)] -> String -> ([(String, Double)])
ngramOut ngramFreqs st = lookupNgram st $ filterList st ngramFreqs



-- function chooser, used in the weighted walk on the ngram above..

chooserFunction :: (Ord a1, Num a1) => [(a2, a1)] -> a1 -> (a2, a1)
chooserFunction ng r = head (filter (\(_,y)-> r < y) list )
  where values = map (fst) ng
        cweights = scanl1 (+) (map snd ng)
        list = zip values cweights


-- can delete this after
-- ngramfunc :: (Eq a , Fractional b, Integral a1) => a -> [([a], a1)] -> [(a, b)]
-- ngramfunc st xs = lookupNgram st (filterList st xs)


-- show all ngrams for the given userword..
lookupNgram :: (Eq a , Fractional b, Integral a1) => a -> [([a], a1)] -> [(a,b)]
lookupNgram st xs = zip (map (head) (getNext (xs))) (getFrequencies total (xs))
  where total = (sum $ map (getFrequency) (filterList st xs))

-- get next function in the expression..
getNext :: [([a], b)] -> [[a]]
getNext st = map (tail) (map (fst) (st))

-- get the array of normalised frequencies for this function..
getFrequencies :: (Fractional b, Integral a1, Integral a2) => a2-> [(a3, a1)] -> [b]
getFrequencies t st = map (\x -> (fromIntegral x) / (fromIntegral t)) ((map getFrequency (st)))


-- filter list based on some string in the text..
filterList :: Eq a => a -> [([a], b)] -> [([a], b)]
filterList st xs  = filter ( \([w, y],z) -> w `elem` [st]) (xs)

-- get first word
getFirst :: Int -> [([a], b)] -> a
getFirst index xs = head $ fst $ (xs!!index)

-- get frequency value
getFrequency :: (a, b) -> b
getFrequency xs = snd $ (xs)


-- read input order as an int
rInt :: String -> Int
rInt = read

-- read input order as Double
rDouble :: String -> Double
rDouble = read


-- ngram calculator.
ngram :: Int -> [a] -> [[a]]
ngram n xs
  | n <= length xs = take n xs : ngram n (drop 1 xs)
  | otherwise = []

-- sort by frequency of ngrams
ngramSort :: Ord a => [a] -> [(a, Int)]
ngramSort f = map (\xs -> (head xs, length xs)) $ group $ sort $ f
