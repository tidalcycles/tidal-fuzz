module Sound.Tidal.Tokeniser where

import Data.List
import Data.Char


-- 1. remove unnecessary punctuation from code..
-- remove parantheses, any other punctuation
removePunc :: [Char] -> [Char]
removePunc xs = [ x | x <- xs, not (x `elem` "\'()$#\n") ]

-- remove $ and # characters , todo unnecessary? can just use filter..
replaceChars :: Char -> Char -> [Char] -> [Char]
replaceChars a b = map (\c -> if c == a then b; else c)

-- 2.  split at new line character,

-- get everything before line break
beforeLine :: String -> String
beforeLine xs = takeWhile (/='\n') xs

-- get every
afterLine :: String -> String
afterLine xs = if (dropWhile (/='\n') xs)==[]
                then []
                else tail(dropWhile (/='\n') xs)


-- reformat to a list of words after Line break
reformatCode :: String -> [String]
reformatCode [] = []
reformatCode xs = beforeLine xs : (reformatCode (afterLine xs))


-- 2. handle line breaks, split into separate patterns
-- replaceChars :: Char -> [Char] -> [Char]
-- replaceChars a = map (\c -> if c == '\n' then a; else c)


-- 3. Treat everything between "" as single token..

-- 3.1 rewriting the words function to break up at
-- how to stop it happening on the second "\ ?

word' s  =  case dropWhile (== '\"') s of
          "" -> []
          s' -> w : words s''
                where (w, s'') =
                       break (== '\"') s'


--- 3.2 split pattern at the "
getBeforePattern :: [Char]-> [Char]
getBeforePattern xs = takeWhile (/='\"') xs


getAfterPattern :: [Char] -> [Char]
getAfterPattern xs = if (dropWhile (/='\"') xs)==[]
                      then []
                      else tail(dropWhile (/='\"') xs)


getComponents :: [Char] -> [[Char]]
getComponents xs = [header, string, footer]
  where header = getBeforePattern (xs)
        string = getBeforePattern (getAfterPattern xs)
        footer = getAfterPattern (getAfterPattern xs)

toWords :: String -> [String]
toWords xs = words xs







-- misc, if using custom line-break up to determine new patterns in the corpus code..?


-- get everything before a \\ character
before :: String -> String
before xs = takeWhile (/='\\') xs

-- get everything after a \\ character
after :: String -> String
after xs = if (dropWhile (/='\\') xs)==[]
             then []
             else tail(dropWhile (/='\\') xs)


-- breakup at tab character..
breakupCode :: String -> [String]
breakupCode [] = []
breakupCode xs = before xs : (breakupCode (after xs))
