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
    ("\"3 4 5\"", Sig [] $ Pattern Int),
    ("1", Sig [] $ Pattern Float),
    ("2", Sig [] $ Pattern Float),
    ("\"3 4 5\"", Sig [] $ Pattern Float),
    ("1", Sig [] $ Float),
    ("2", Sig [] $ Float),
    ("\"3 4 5\"", Sig [] $ Float),
    ("1", Sig [] $ Float),
    ("2", Sig [] $ Float),
    ("\"3 4 5\"", Sig [] $ Float),
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

{-
 1/ Start with target (e.g. Pattern OSC)
 2/ Find all values that could produce that target (resolving type in the process? )
 3/ Pick one (e.g. at random)
 4/ Recurse to missing arguments
-}

-- Picks the first value
walk :: Sig -> IO (String, [String])
walk target = do r <- randomIO
                 when (null $ options target) $ error ("No options meet " ++ show target)
                 let (name, s@(Sig _ t)) = pick r (options target)
                     history = [name]
                 -- putStrLn $ n ++ " :: " ++ show s
                 -- putStrLn $ name -- ++ " ("
                 result <- walkFunction history 0 $ s
                 return (name ++ " " ++ fst result, snd result )
                 -- putStrLn $ ")"


{-
weightedWalkFunction :: (String -> [(String, Double)]) -> [String] -> Sig -> IO ()
weightedWalkFunction ngramfunc history target = ..
-}

walkFunction :: [String] -> Int -> Sig -> IO (String, [String])
-- We've matched a function
walkFunction history depth (Sig ps t@(F arg result)) =
  do r <- randomIO
     -- Choose from the possible options, with types resolved to match the context
     let (name,s@(Sig ps' t')) = pick r (options $ Sig ps arg)
         history' = (name:history)
     -- Print the name and type of the option we've picked for the argument
     -- putStrLn $ n ++ " :: " ++ show s
     -- Recurse with the argument, in case it's a function
     -- putStrLn $ indent ++ name -- ++ " [arity: " ++ show (arity t') ++ "]"
     (res, his) <- if (arity arg < arity t')
                   then (walkFunction (history') (depth+1) $ s)
                   else (return (" ", history'))
     -- Recurse with the result of the function, in case it's also a function (i.e. we have a multi-argument function)
     -- TODO - maybe the ps will now be wrong?
     (res', his') <- if (isFunction result)
                     then (walkFunction (his) depth $ Sig ps result)
                     else (return (" ", his))
     return (name ++ " " ++ res ++ " " ++ res', his')
  where indent = replicate depth ' '
walkFunction _ _ _ = return (" ", [])

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
