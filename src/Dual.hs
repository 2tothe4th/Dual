module Dual where
import Data.Functor
--https://en.wikipedia.org/wiki/Dual_number
--Another video

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

instance Num a => Num (Dual a) where
    (Dual a b) + (Dual c d) = Dual (a + c) (b + d)
    (Dual a b) * (Dual c d) = Dual (a * c) (a * d + b * c)
    negate z = negate <$> z
    abs = undefined
    signum z = signum <$> z
    fromInteger x = Dual (fromInteger x) 0

conjugateDual :: Num a => Dual a -> Dual a
conjugateDual (Dual a b) = Dual a (-b)

instance (Eq a, Fractional a) => Fractional (Dual a) where 
    Dual 0 b / Dual 0 d = Dual (b / d) 0
    Dual _ _ / Dual 0 _ = error "Division of the form (a + bε)/(dε) where a is non-zero is undefined."
    Dual a b / Dual c d = Dual (a / c) ((b * c - a * d) / (c * c))
    fromRational = makeDual

derive :: (Eq a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Dual a)
derive f x = (f (x + epsilon) - f x) / epsilon