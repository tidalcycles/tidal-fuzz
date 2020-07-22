module Sound.Tidal.Types where

import Data.List (intersectBy, nub)
import Data.Maybe (fromMaybe, catMaybes)
import System.Random
import Control.Monad

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
  Int == Int = True
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

functions :: [(String, Sig)]
functions = 
  [--("+", numOp),
   --("-", numOp),
   --("/", floatOp),
   --("*", numOp),
   --("#", Sig [] $ F (Pattern Osc) (F (Pattern Osc) (Pattern Osc))),
   --("striate", Sig [] $ F (Pattern Int) (F (Pattern Osc) (Pattern Osc))),
   --("chop", Sig [] $ F (Pattern Int) (F (Pattern Osc) (Pattern Osc))),
   -- ("floor", Sig [] $ F Float Int),
    ("sine", floatPat),
   ("run", Sig [] $ F (Pattern Int) (Pattern Int)),
   --("fmap", mapper),
   --("<$>", mapper),
   --("<*>", Sig [WildCard, WildCard] $ F (Pattern $ F (Param 0) (Param 1)) (F (Pattern (Param 0)) (Pattern (Param 1)))),
   ("sound", stringToOsc),
   ("vowel", stringToOsc),
   ("shape", floatToOsc),
   ("speed", floatToOsc),
   ("delay", floatToOsc),
   ("pan", floatToOsc),
   ("every", Sig [WildCard] $ F (Pattern Int) 
             (F (F (Pattern $ Param 0) (Pattern $ Param 0)) 
                (Pattern $ Param 0)
             )
   ),
   -- ("fast", Sig [WildCard] $ F (Pattern Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   {-
   ("overlay", Sig [WildCard] $ F (Pattern $ Param 0) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   ("append", Sig [WildCard] $ F (Pattern $ Param 0) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   ("silence", Sig [] $ Pattern WildCard),
   ("density", Sig [WildCard] $ F (Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   ("slow", Sig [WildCard] $ F (Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
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
   ("jux", Sig 
           []  
           (F (F (Pattern Osc) (Pattern Osc))
            (F (Pattern Osc) (Pattern Osc))
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
    ("rev", Sig [WildCard] $ F (Pattern $ Param 0) (Pattern $ Param 0)),
    ("1", Sig [] $ Pattern Int),
    ("2", Sig [] $ Pattern Int),
    ("3 4 5", Sig [] $ Pattern Int),
    ("1", Sig [] $ Pattern Float),
    ("2", Sig [] $ Pattern Float),
    ("3 4 5", Sig [] $ Pattern Float),
    ("1", Sig [] $ Float),
    ("2", Sig [] $ Float),
    ("3 4 5", Sig [] $ Float),
    ("1", Sig [] $ Float),
    ("2", Sig [] $ Float),
    ("3 4 5", Sig [] $ Float),
    ("\"bd sn\"", Sig [] $ Pattern String)
  ]
  where numOp = Sig [number] $ F (Param 0) $ F (Param 0) (Param 0)
        floatOp = Sig [] $ F (Pattern Float) (F (Pattern Float) (Pattern Float))
        floatPat = Sig [] $ Pattern Float
        mapper = Sig [WildCard, WildCard] $ F (F (Param 0) (Param 1)) $ F (Pattern (Param 0)) (Pattern (Param 1))
        stringToOsc = Sig [] $ F (Pattern String) (Pattern Osc)
        floatToOsc = Sig [] $ F (Pattern Float) (Pattern Osc)
        number = OneOf [Float, Int, Pattern Float, Pattern Int]

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

-- can function produce target function (and how)?
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

{-
canAs target@(Sig pA (Param a)) (Sig pB b)
  | matches = Just $ Sig (setAt pB b $ resolveParam pA a) (Param b)
  | otherwise = Nothing
  where matches = fits (Sig pA (Param a)) (Sig pB b)
-}

canAs target@(Sig _ a) (Sig _ b) | a == b = Just target

canAs _ _ = Nothing

resolveParam :: [Type] -> Type -> Type
resolveParam ps (Param n) = ps !! n
resolveParam ps (F a b) = F (resolveParam ps a) (resolveParam ps b)
resolveParam ps (OneOf ts) = OneOf $ map (resolveParam ps) ts
resolveParam ps (Pattern t) = Pattern $ resolveParam ps t
resolveParam ps (List t) = List $ resolveParam ps t
resolveParam _ t = t

{-
normaliseType :: Type -> Type
normaliseType x@(OneOf []) = x -- shouldn't happen..
normaliseType (OneOf (x:[])) = x
normaliseType (OneOf xs) = OneOf $ nub xs
normaliseType x = x

setParam :: Int -> Type -> Type -> Type
setParam n to (F a b) = F (setParam n to a) (setParam n to b)
setParam n to (OneOf ts) = OneOf $ map (setParam n to) ts
setParam n to (Pattern t) = Pattern $ setParam n to t)
setParam n to from@(Param n') | n == n' =  to
                              | otherwise = from
setParam n to (List t) = List $ setParam n to t
setParam n to (SimpleList t) = List $ setParam n to t
setParam n _ from = from

normaliseSig :: Sig -> Sig
normaliseSig (Sig ps t) = foldr f t eps
  where eps = zip ([0 ..], map normaliseType ps)
        f t (n,OneOf _)) = t
        f t (n,Wildcard) = t
-}

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

{-
 1/ Start with target (e.g. Pattern OSC)
 2/ Find all values that could produce that target (resolving type in the process? )
 3/ Pick one (e.g. at random)
 4/ Recurse to missing arguments
-}

walk target = do r <- randomIO
                 when (null $ options target) $ error ("No options meet " ++ show target)
                 let (n,s) = pick r (options target)
                 putStrLn $ n ++ " :: " ++ show s
                 nxt s

{-
pan :: (p [f] -> p [osc])
fast :: [f] => (p [f] -> (p [param#0] -> p [param#0]))
2 :: [f] => p [f]

rev :: [osc] => (p [param#0] -> p [param#0])

options $ Sig [] (Pattern $ Osc)
options $ Sig [Osc] (Pattern $ Param 0)

-}

-- We've matched a function
nxt (Sig ps (F arg result)) =
  do r <- randomIO
     -- Choose from the possible options, with types resolved to match the context
     let (n,s@(Sig ps' t')) = pick r (options $ Sig ps arg)
     -- Print the name and type of the option we've picked for the argument
     putStrLn $ n ++ " :: " ++ show s
     -- Recurse with the argument, in case it's a function
     nxt $ s
     -- Recurse with the result of the function, in case it's also a function (i.e. we have a multi-argument function)
     -- TODO - maybe the ps will now be wrong?
     nxt $ Sig ps result
nxt _ = return ()

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

