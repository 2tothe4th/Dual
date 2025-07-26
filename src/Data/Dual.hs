module Data.Dual where
import Data.Functor
-- https://en.wikipedia.org/wiki/Dual_number
-- https://math.stackexchange.com/questions/1914591/dual-number-ab-varepsilon-raised-to-a-dual-power-e-g-ab-varepsilon
-- https://www.tiktok.com/@maths_visual/video/7530507874926136606

data Dual a = Dual { dualReal :: a, dualEpsilon :: a} deriving Eq

toDual :: Num a => a -> Dual a
toDual x = Dual x 0

dualOne :: Num a => Dual a
dualOne = Dual 1 0

epsilon :: Num a => Dual a
epsilon = Dual 0 1

instance (Ord a, Num a, Show a) => Show (Dual a) where
    show (Dual a 0) = show a
    show (Dual 0 b) = show b ++ "ε"
    show (Dual a b) = concat [show a, if b >= 0 then " + " else " - ", show (abs b), "ε"]

instance Functor Dual where
    fmap function (Dual a b) = Dual (function a) (function b)

instance Applicative Dual where
    (Dual f g) <*> (Dual a b) = Dual (f a) (g b)
    pure x = Dual x x

instance Monad Dual where
    (Dual a _) >>= function = function a

instance Num a => Num (Dual a) where
    (Dual a b) + (Dual c d) = Dual (a + c) (b + d)
    (Dual a b) * (Dual c d) = Dual (a * c) (a * d + b * c)
    negate = (negate <$>)
    abs (Dual a _) = Dual a 0 -- a + bε = ae^(bε/a), this defintion of abs is very subjective
    signum = (signum <$>)
    fromInteger = toDual . fromInteger

conjugateDual :: Num a => Dual a -> Dual a
conjugateDual (Dual a b) = Dual a (-b)

instance (Eq a, Fractional a) => Fractional (Dual a) where 
    Dual 0 b / Dual 0 d = Dual (b / d) 0
    Dual _ _ / Dual 0 _ = error "Division of the form (a + bε)/(dε) where a is non-zero is undefined."
    Dual a b / Dual c d = Dual (a / c) ((b * c - a * d) / (c * c))
    fromRational = toDual . fromRational

instance (Eq a, Floating a) => Floating (Dual a) where
    exp (Dual a b) = let scalar = exp a in Dual scalar (scalar * b)
    log (Dual a b) = Dual (log a) (b / a)
    pi = toDual pi
    -- https://en.wikipedia.org/wiki/Hyperbolic_functions
    sinh z = 0.5 * (exp z - exp (-z))
    cosh z = 0.5 * (exp z + exp (-z))
    tanh z = (exp z - exp (-z)) / (exp z + exp (-z))

derive :: (Eq a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Dual a)
derive f x = (f (x + epsilon) - f x) / epsilon