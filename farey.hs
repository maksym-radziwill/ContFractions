import Data.List
import System.Environment
    
data Tree a = Branch (Tree a) (Tree a) a 
              deriving (Show)

data Fraction = Fraction (Integer, Integer)

instance Eq Fraction where
    (==) (Fraction (a,b)) (Fraction (c,d)) = a*d == b*c

instance Num Fraction where
    (+) (Fraction (a,b)) (Fraction (c,d)) = Fraction (a*d + c*b, b*d)
    (-) (Fraction (a,b)) (Fraction (c,d)) = Fraction (a*d - c*b, b*d)
    (*) (Fraction (a,b)) (Fraction (c,d)) = Fraction (a*c, b*d)
    (abs) (Fraction (a,b)) = Fraction (abs a, abs b)
    (signum) (Fraction (a,b)) = Fraction ((signum a)*(signum b), 1)
    (fromInteger) n = Fraction (n,1)

piFrac :: Integer -> Fraction
piFrac n = ((Fraction (2,1))*(prod 1 3 n) - 3)

prod :: Integer -> Integer -> Integer -> Fraction 
prod a b n | a > n = 1
           | otherwise = (Fraction (1,1)) + (Fraction (a, b))*(prod (a + 1) (b + 2) n)
                                             
instance Ord Fraction where
    (<=) (Fraction (a,b)) (Fraction (c,d)) = a*d <= b*c
                       
type FareyTree = Tree Fraction

instance Show Fraction where
    show (Fraction (a,b)) = let d = gcd a b
                            in  (show (div a d)) ++ "/" ++ (show (div b d))
    
instance Functor Tree where
    fmap f (Branch a b c) = Branch (fmap f a) (fmap f b) (f c)

data Search = L | R
              deriving (Show, Eq)

toFraction :: Integer -> Fraction
toFraction a = Fraction (a, 10^(log10 a))
    where log10 a = toInteger $ length $ takeWhile (/= 0) $ iterate (\x -> div x 10) a

parse :: String -> (Integer, Fraction)
parse str = let u = takeWhile (\x -> x /= '.') str
                v = drop 1 $ dropWhile (\x -> x /= '.') str
            in  (read u :: Integer, toFraction (read v :: Integer))
                    
fareySearch :: Fraction -> FareyTree -> [Search]
fareySearch a (Branch c d e) | a > e  = R : (fareySearch a d)
                             | a < e  = L : (fareySearch a c)
                             | a == e = []

reduceFraction :: Fraction -> Fraction
reduceFraction (Fraction (a,b)) = let d = gcd a b
                                  in  Fraction (div a d, div b d)
                                        
continuedFrac :: Fraction -> [Int]
continuedFrac a = fmap length $ group $ fareySearch (reduceFraction a) fareyFullTree

fareyFullTree :: FareyTree
fareyFullTree = fareyTree (Fraction (0,1)) (Fraction (1,0))
                  
-- fareyTree left right
fareyTree (Fraction (a,n)) (Fraction (a',n')) =
    let m = Fraction (a + a', n + n')
    in  Branch (fareyTree (Fraction (a,n)) m) (fareyTree m (Fraction (a', n'))) m

log2 :: Integer -> Integer
log2 a = toInteger $ length $ takeWhile (/= 0) $ iterate (\x -> div x 2) a
        
main :: IO ()
main =
    do
      [str] <- getArgs
      if str == "pi"
      then do
        putStrLn "how many digits? "
        w <- getLine
        print $ show $ continuedFrac $ piFrac $ log2 (10^(read w :: Integer))
      else do
        putStrLn (((show . fst . parse) str) ++ " + " ++
                  ((show . continuedFrac . snd . parse) str))
