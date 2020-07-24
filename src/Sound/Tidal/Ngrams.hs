module Sound.Tidal.Ngrams where

import System.IO
import Data.List
import Data.Ord
import Data.Char
import Data.String


main = do
    handle <- openFile "Sound/Tidal/tidal-input.txt" ReadMode
    contents <- hGetContents handle
    let order = 2 -- keep as bigrams for now..
    -- remove any unwanted punctuation, reformat each word into separate line
    let reformat1 = toWords $ removePunc contents
    -- get frequency ngrams of functions used..
    let ngramFreqs = ngramSort $ ngram order $ reformat1
    userword <- getLine
    let ngramfunc = lookupNgram userword $ filterList userword ngramFreqs
    print (ngramfunc) -- to do: remove total as variable..
    hClose handle

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


-- convert a string to an array of its words
toWords :: String -> [String]
toWords s =  words s


-- remove unwanted punctuation
removePunc :: [Char] -> [Char]
removePunc xs = [ x | x <- xs, not (x `elem` "*\'()") ]


-- get everything before a space character
before :: String -> String
before xs = takeWhile (/=' ') xs

-- get everything after a space character
after :: String -> String
after xs = if (dropWhile (/=' ') xs)==[]
             then []
             else tail(dropWhile (/=' ') xs)


-- get everything before line break
beforeLine :: String -> String
beforeLine xs = takeWhile (/='\n') xs

-- get everything after line break..
afterLine :: String -> String
afterLine xs = if (dropWhile (/='\n') xs)==[]
                 then []
                 else tail(dropWhile (/='\n') xs)


-- reformat to a list of words after Line break
reformatCode :: String -> [String]
reformatCode [] = []
reformatCode xs = beforeLine xs : (reformatCode (afterLine xs))

-- breakup at the
breakupCode :: String -> [String]
breakupCode [] = []
breakupCode xs = before xs : (breakupCode (after xs))

-- ngram calculator.
ngram :: Int -> [a] -> [[a]]
ngram n xs
  | n <= length xs = take n xs : ngram n (drop 1 xs)
  | otherwise = []

-- sort by frequency of ngrams
ngramSort :: Ord a => [a] -> [(a, Int)]
ngramSort f = map (\xs -> (head xs, length xs)) $ group $ sort $ f
