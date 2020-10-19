module Sound.Tidal.Types where

import Data.List (intersectBy, nub, (\\))
import Data.Maybe (fromMaybe, catMaybes, fromJust, isJust)
import System.Random
import Control.Monad
import Sound.Tidal.Ngrams
import GHC.Float


data Type =
  F Type Type
  | String
  | Float
  | Int
  | Bool
  | Osc
  | OscStream
  | OneOf [Type]
  | Pattern Type
  | WildCard
  | Param Int
  | List Type
  | SimpleList Type


instance Eq Type where
  F a a' == F b b' = and [a == b,
                          a' == b'
                         ]
  String == String = True
  Float == Float = True
  Bool == Bool = True
  Osc == Osc = True
  OscStream == OscStream = True
  OneOf as == OneOf bs = as == bs
  Pattern a == Pattern b = a == b
  WildCard == WildCard = True
  Param a == Param b = a == b
  List a == List b = a == b
  SimpleList a == SimpleList b = a == b
  _ == _ = False

-- Type signature
data Sig = Sig {params :: [Type],
                is :: Type
               }
           deriving Eq

instance Show Type where
 show (F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
 show String = "s"
 show Float = "f"
 show Bool = "#"
 show Int = "i"
 show Osc = "osc"
 show (OneOf ts) = "?" ++ (show ts)
 show (Pattern t) = "p [" ++ (show t) ++ "]"
 show WildCard = "*"
 show (Param n) = "param#" ++ (show n)
 show (OscStream) = "stream"
 show (List t) = "list [" ++ (show t) ++ "]"
 show (SimpleList t) = "simplelist [" ++ (show t) ++ "]"


instance Show Sig where
   show s = ps ++ (show $ is s)
     where ps | params s == [] = ""
              | otherwise = show (params s) ++ " => "


data Construct = Construct {context :: [String],
                             csig :: Sig
                            }
data Code = Arg Code Code
           | Parens Code
           | Name String
           -- deriving Show

instance Show Code
   where -- show (Arg a (Parens b@(Arg _ _))) = show a ++ " (" ++ show b ++ ")"
         -- show (Arg a (Parens b)) = show a ++ " $ " ++ show b
         show (Arg a b) = show a ++ " " ++ show b
         -- show (Parens a@(Arg _ (Arg _ _))) = "(" ++ show a ++ ")"
         -- show (Arg a (Parens b) = show a ++ " (" ++ show b ++ ")"
         show (Parens a) = "(" ++ show a ++ ")"
         show (Name s) = s

functions :: [(String, Sig)]
functions =
   [("(+)", numOp),
    --("-", numOp),
    --("/", floatOp),
    --("*", numOp),
    ("(#)", Sig [] $ F (Pattern Osc) (F (Pattern Osc) (Pattern Osc))),
    --("striate", Sig [] $ F (Pattern Int) (F (Pattern Osc) (Pattern Osc))),
    ("chop", Sig [] $ F (Pattern Int) (F (Pattern Osc) (Pattern Osc))),
    -- ("floor", Sig [] $ F Float Int),
    ("sine", floatPat),
    ("run", Sig [] $ F (Pattern Int) (Pattern Int)),
    --("fmap", mapper),
    --("<$>", mapper),
    --("<*>", Sig [WildCard, WildCard] $ F (Pattern $ F (Param 0) (Param 1)) (F (Pattern (Param 0)) (Pattern (Param 1)))),
    ("sound", stringToOsc),
    -- ("vowel", stringToOsc),
    -- ("shape", floatToOsc),
    -- ("speed", floatToOsc),
    -- ("delay", floatToOsc),
    -- ("pan", floatToOsc),
    ("every", Sig [WildCard] $ F (Pattern Int)
              (F (F (Pattern $ Param 0) (Pattern $ Param 0))
                 (F (Pattern $ Param 0) (Pattern $ Param 0))
              )
    ),
    -- ("instantgabba", Sig [] $ Pattern Osc),
    ("fast", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("slow", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    -- ("fast", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    {-
    ("overlay", Sig [WildCard] $ F (Pattern $ Param 0) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("append", Sig [WildCard] $ F (Pattern $ Param 0) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("silence", Sig [] $ Pattern WildCard),
    ("iter", Sig [WildCard] $ F (Pattern Int) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("spin", Sig [] $ F (Int) (F (Pattern Osc) (Pattern $ Osc))),
    ("stut", Sig [] $ F (Pattern Int) $ F (Pattern Float) $ F (Pattern Float) $ (F (Pattern Osc) (Pattern Osc))),
    ("<~", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("~>", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
    ("chunk", Sig [WildCard] $ F (Pattern Int)
              (F (F (Pattern $ Param 0) (Pattern $ Param 0))
                 (F (Pattern $ Param 0) (Pattern $ Param 0))
              )
    ),
    ("superimpose", Sig []
                        (F (F (Pattern Osc) (Pattern Osc))
                         (F (Pattern Osc) (Pattern Osc))
                        )
    ),
    ("wedge", Sig [WildCard] $ F (Float) (F (Pattern $ Param 0) (F (Pattern $ Param 0) (Pattern $ Param 0)))),
    ("brak", Sig [WildCard] $ F (Pattern $ Param 0) (Pattern $ Param 0)),
    ("pick", Sig [] $ F String (F Int String)),
    ("]", Sig [OneOf [String,Int,Float]] (List (Param 0))),
    ("[", Sig [OneOf [String,Int,Float]] (F (List (Param 0)) (Pattern (Param 0))))
 -}
    ("jux", Sig
            []
            (F (F (Pattern Osc) (Pattern Osc))
             (F (Pattern Osc) (Pattern Osc))
            )
    ),
     ("rev", Sig [WildCard] $ F (Pattern $ Param 0) (Pattern $ Param 0)),
     ("1", Sig [] $ Pattern Int),
     ("2", Sig [] $ Pattern Int),
     ("\"3 4 5\"", Sig [] $ Pattern Int),
     ("1", Sig [] $ Pattern Float),
     ("2", Sig [] $ Pattern Float),
     ("\"3 4 5\"", Sig [] $ Pattern Float),
     {-
     ("1", Sig [] $ Float),
     ("2", Sig [] $ Float),
     ("\"3 4 5\"", Sig [] $ Float),
     ("1", Sig [] $ Float),
     ("2", Sig [] $ Float),
     ("\"3 4 5\"", Sig [] $ Float),
-}
     ("\"bd sn\"", Sig [] $ Pattern String)
   ]
   where numOp = Sig [OneOf[Float,Int]] $ F (Pattern $ Param 0) $ F (Pattern $ Param 0) (Pattern $ Param 0)
         floatOp = Sig [] $ F (Pattern Float) (F (Pattern Float) (Pattern Float))
         floatPat = Sig [] $ Pattern Float
         mapper = Sig [WildCard, WildCard] $ F (F (Param 0) (Param 1)) $ F (Pattern (Param 0)) (Pattern (Param 1))
         stringToOsc = Sig [] $ F (Pattern String) (Pattern Osc)
         floatToOsc = Sig [] $ F (Pattern Float) (Pattern Osc)
--         number = OneOf [Pattern Float, Pattern Int]
         number = Pattern (OneOf[Float,Int])


showFunctions :: String
showFunctions = concatMap f functions
  where f (s, t) = s ++ " :: " ++ show t ++ "\n"

showNames :: String
showNames = concatMap f functions
  where f (s, t) = s ++ ", "

stringToType :: String -> Type
stringToType [] = String
stringToType s = Pattern t -- TODO OneOf [t, Pattern t]
  where t = scanType Int s
        scanType t [] = t
        scanType Int ('.':[]) = String
        scanType Int (c:s) | elem c ['0' .. '9'] = scanType Int s
                           | c == '.' = scanType Float s
                           | otherwise = String
        scanType Float (c:s) | elem c ['0' .. '9'] = scanType Float s
                             | otherwise = String


stringToSig :: String -> Sig
stringToSig s = fromMaybe def $ lookup s functions
   where def = Sig [] (stringToType s)


fits :: Sig -> Sig -> Bool
fits (Sig _ WildCard) _ = True
fits _ (Sig _ WildCard) = True

fits (Sig pA (F a a')) (Sig pB (F b b')) =
 (fits (Sig pA a) (Sig pB b)) && (fits (Sig pA a') (Sig pB b'))

fits (Sig pA (OneOf as)) (Sig pB (OneOf bs)) =
 intersectBy (\a b -> fits (Sig pA a) (Sig pB b)) as bs /= []

fits (Sig pA (OneOf as)) (Sig pB b) =
 or $ map (\x -> fits (Sig pA x) (Sig pB b)) as

fits (Sig pA a) (Sig pB (OneOf bs)) =
 or $ map (\x -> fits (Sig pA a) (Sig pB x)) bs

fits (Sig pA (Pattern a)) (Sig pB (Pattern b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA (List a)) (Sig pB (List b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA a) (Sig pB (Param b)) = fits (Sig pA a) (Sig pB (pB !! b))
fits (Sig pA (Param a)) (Sig pB b) = fits (Sig pA (pA !! a)) (Sig pB b)

fits (Sig _ Float) (Sig _ Float)   = True
fits (Sig _ Int) (Sig _ Int)       = True
fits (Sig _ String) (Sig _ String) = True
fits (Sig _ OscStream) (Sig _ OscStream) = True
fits (Sig _ Osc) (Sig _ Osc) = True

fits _ _ = False


-- Will either return the target, or a function that (ultimately)
-- returns the target, or nothing
canAs :: Sig -> Sig -> Maybe Sig

-- can function b produce target function a (and how)?
canAs (Sig pA (F a a')) (Sig pB (F b b')) =
  do -- fit argument
     (Sig _ arg) <- (canAs (Sig pA a) (Sig pB b))
     -- fit result
     (Sig _ result) <- canAs (Sig pA a') (Sig pB b')
     return $ Sig pA (F arg result)

-- can function produce target value (and how)?
canAs target@(Sig pA a) (Sig pB (F b b')) =
  do (Sig pX x) <- canAs target (Sig pB b')
     return $ Sig pX $ F b x

-- A wildcard can produce anything
canAs target (Sig _ WildCard) = Just target

-- Check target is a subset
canAs target@(Sig pA (OneOf as)) (Sig pB (OneOf bs))
  | isSubset = Just target
  | otherwise = Nothing
  where isSubset = length (intersectBy (\a b -> fits (Sig pA a) (Sig pB b)) as bs) == length as

-- Check target is in the 'OneOf'
canAs target (Sig pB (OneOf bs)) | isIn = Just target
                                 | otherwise = Nothing
  where isIn = or $ map (\x -> fits target (Sig pB x)) bs

--
canAs (Sig pA (Pattern a)) (Sig pB (Pattern b)) =
  do (Sig ps t) <- canAs (Sig pA a) (Sig pB b)
     return $ Sig ps (Pattern t)

canAs (Sig pA (List a)) (Sig pB (List b)) =
  do (Sig ps t) <- canAs (Sig pA a) (Sig pB b)
     return $ Sig ps (List t)

{-
-- Check target matches the parameter
canAs target@(Sig pA a) from@(Sig pB (Param b))
  = canAs target (Sig (setAt pB b $ resolveParam pA a))
-}

canAs target@(Sig pA a) from@(Sig pB (Param b))
  -- If they match, resolve the parameter to the target
  | matches = Just $ Sig (setAt pB b $ resolveParam pA a) (Param b)
  | otherwise = Nothing
  where matches = fits target from

canAs target@(Sig pA (Param a)) from@(Sig pB b)
  -- If they match, resolve the parameter to the 'from'
  | matches = Just $ Sig (setAt pA a $ resolveParam pB b) (Param a)
  | otherwise = Nothing
  where matches = fits target from

canAs target@(Sig _ a) (Sig _ b) | a == b = Just target

canAs _ _ = Nothing

resolveParam :: [Type] -> Type -> Type
resolveParam ps (Param n) = ps !! n
resolveParam ps (F a b) = F (resolveParam ps a) (resolveParam ps b)
resolveParam ps (OneOf ts) = OneOf $ map (resolveParam ps) ts
resolveParam ps (Pattern t) = Pattern $ resolveParam ps t
resolveParam ps (List t) = List $ resolveParam ps t
resolveParam _ t = t


setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

input :: Sig -> Sig
input (Sig ps (F x _)) = Sig ps x
input _ = error "No input to non-function"

output :: Sig -> Maybe Sig
output (Sig ps (F _ x)) = Just $ Sig ps x
output _ = Nothing

fitsOutput :: Sig -> Sig -> Bool
fitsOutput target t | fits t target = True
                    | otherwise = maybe False (fitsOutput target) (output t)


debug = True

walk:: Sig -> IO Code
walk sig = do
              (history, Parens code) <- walk' [] sig
              return (code)


walk' :: [String] -> Sig -> IO ([String], Code)
walk' history target = do r <- randomIO
                          when (null $ options target) $ error ("No options meet " ++ show target)
                          let opts = (options target)
                          let (name, match) = pick r opts
                          (history', code) <- supply (name:history) (arity (is match)
                                      - arity (is target)) (Name name) match
                          return $ (history', parenthesise code)
      where parenthesise code@(Arg _ _) = Parens code
            parenthesise code = code

supply :: [String] -> Int -> Code -> Sig -> IO ([String], Code)
supply history 0 code _ = return (history, code)
supply history n code (Sig ps (F arg result))
  = do (history', code') <- walk' history (Sig ps arg)
       (history'', code'') <- supply history' (n-1) code' (Sig ps result)
       return $ (history'', Arg (code) (code''))

  {-
      weighted walk..
      1. randomly pick the first element of the walk
      2. generate next possible options using ngram
      3. choose based on probabilities which function next
      4. recurse? how to integrate with type checking?
      5. picking something at random not in the ngram <- how likely.
  -}

wWalk :: Sig -> IO Code
wWalk sig = do
              -- get ngrams from corpus
              ngramFreqs <- lookupT
              -- recurse
              (history, Parens code) <- wWalk' [] 1 ngramFreqs sig
              return (code)

-- every 3 rev (every 4 (fast (rev (every 3 rev))

wWalk' :: [String] -> Int -> [([String], Int)] -> Sig -> IO ([String], Code)
wWalk' history depth ngramFreqs target = do
                          when debug $ putStrLn $ "wWalk': " ++ show target
                          r <- randomIO
                          -- Get all the syntactically correct possibilities for next function/value
                          let opts = options target
                          when (null opts) $ error ("No options meet " ++ show target)
                          -- Weight the possibilities based on ngrams
                          wOpts <- weightedOpts history opts ngramFreqs
                          -- Constrain (TODO - would it be better to filter before weighting?)
                          let wOpts' = filterBloat history wOpts
                              wOpts'' = rollOff depth wOpts'
                          -- Pick one
                          let (name, match, prob) = weightedPick r wOpts''
                          when debug $ putStrLn $ show (name, match, prob)
                          -- Recurse on any needed parameters
                          (history', code) <- wSupply (name:history) depth ngramFreqs (arity (is match)
                                      - arity (is target)) (Name name) match
                          return $ (history', parenthesise code)
      where parenthesise code@(Arg _ _) = Parens code
            parenthesise code = code


filterBloat :: [String] -> [(String, Sig, Double )] -> ([(String, Sig, Double )])
filterBloat [] wOpts = wOpts
filterBloat history wOpts = bloat (head history) wOpts

-- bloat :: String -> [([Char], b, c)] -> [([Char], b, c)]
-- bloat :: String -> [([Char], a, b)] -> IO ([([Char], b, c)])
bloat st xs = [c | c <- xs, fs c /= st]

wSupply :: [String] -> Int -> [([String], Int)] -> Int -> Code -> Sig -> IO ([String], Code)
wSupply history _ ngramFreqs 0 code _ = return (history, code)
wSupply history depth ngramFreqs n code (Sig ps (F arg result))
  = do
       -- when debug $ putStrLn "wSupply'"
       -- 
       (history', code') <- wWalk' history (depth + 1) ngramFreqs (Sig ps arg)
       (history'', code'') <- wSupply history' depth ngramFreqs (n-1) code' (Sig ps result)
       return $ (history'', Arg (code) (code''))

rollOff :: Int -> [(String, Sig, Double)] -> [(String, Sig, Double)]
rollOff depth wOpts = map f wOpts
  where f (name, sig, weight) = (name, sig, mungeWeight (arity $ is sig) weight)
        mungeWeight 0 w = w
        -- TODO - adjust this curve..
        mungeWeight a w = w * (1/((fromIntegral a) * (fromIntegral depth)))
        
weightedOpts :: [String] -> [(String, Sig)] -> [([String], Int)] -> IO [(String, Sig, Double )]
weightedOpts history opts ngramFreqs = do
                              -- adherence <- getLine
                              -- when debug $ putStrLn "weightedOpts"
                              let dfltWeights = 1 / (fromIntegral (length opts))
                                  dfltArray = map (* dfltWeights) (take (length opts) [1,1..])
                              if (null history) then do
                                let out = zip3 (map fst opts) (map snd opts) (dfltArray)
                                return (out)
                              else do
                                let ngram = ngramOut ngramFreqs (head history)
                                    out' = map (\(name, sig) -> (name, sig, lookup (name) (ngram))) opts
                                    values = catMaybes $ map td out'
                                    summer = (average values) -- * (rDouble adherence) -- no corpus giving this value for rev rev
                                    out'' = map (\(name, sig, weight) -> (name, sig, fromMaybe summer weight)) out'
                                return (out'')

average xs
  | xs == [] = 0.5
  | otherwise = sum xs / (fromIntegral $ length xs)


weightedPick :: (Ord c, Num c) => c -> [(a, b, c)] -> (a, b, c)
weightedPick f [] = error $ "No options to choose from"
weightedPick f xs = head (filter (\(_, _, y)-> f' < y) list )
  where values = map (fs) xs
        f' = f * (sum (map td xs))
        sigs = map (sn) xs
        cweights = scanl1 (+) (map td xs)
        list = zip3 values sigs cweights

fs :: (a, b, c) -> a
fs (a, _, _) = a

sn :: (a,b,c ) -> b
sn (_, b, _) = b

td :: (a, b, c) -> c
td (_, _, c) = c



-- difference of options not in ngram
notInNgram :: [(String, Sig)] -> [(String, Double)] -> [String]
notInNgram opts ngram = (map fst opts) \\ (map fst ngram)

-- get difference of ngram not in options
notInOptions :: [(String, Sig)] ->[(String, Double)] ->  [String]
notInOptions opts ngram = (map fst ngram) \\ (map fst opts)

-- idempotent functions
-- f(f(x)) = f(x)
-- e.g. every 1 (fast 2 ) = fast 2


-- filter :: [String] -> [(String, Sig, Double)] -> ??
-- filter history wOpts = do
                          -- let this = head history
                          -- if fst wOpts == this then  -- multiply the weights to reduce likelihood of occuring


-- filterRev :: []
-- filterRev history wOpts = do
--                             let this = head history
--                             if (this == "rev")
--                               then return (map (td * 0) wOpts)
--                               else return (wOpts)


simplifyType :: Type -> Type
simplifyType x@(OneOf []) = x -- shouldn't happen..
simplifyType (OneOf (x:[])) = x
simplifyType (OneOf xs) = OneOf $ nub xs
simplifyType x = x

pick :: Float -> [a] -> a
pick f [] = error "no options to pick from"
pick f xs = xs !! (floor (fromIntegral (length xs) * f))

options :: Sig -> [(String, Sig)]
options target = possible
  where
    possible = catMaybes $ map look functions
    look (n,s) = do s' <- canAs target s
                    return $ (n,s')

showopts :: Sig -> String
showopts target = concatMap (\(n,s) -> n ++ ": " ++ show s ++ "\n") (options target)

isFunction :: Type -> Bool
isFunction (F _ _) = True
isFunction _ = False

arity :: Type -> Int
arity (F _ b) = (arity b) + 1
arity _ = 0
