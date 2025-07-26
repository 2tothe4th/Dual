module Data.Dual where
import Linear
import Control.Lens

data Dual a = Dual { real :: a, nonreal :: a } deriving Eq

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
    fmap f (Dual a b) = Dual (f a) (f b)

-- Identical to Complex
instance Applicative Dual where
    (Dual f g) <*> (Dual a b) = Dual (f a) (g b)
    pure x = Dual x x

-- Also identical to Complex
instance Monad Dual where
    (Dual a b) >>= f = Dual (real $ f a) (nonreal $ f b)

instance Num a => Num (Dual a) where
    (Dual a b) + (Dual c d) = Dual (a + c) (b + d)
    (Dual a b) * (Dual c d) = Dual (a * c) (a * d + b * c)
    negate = (negate <$>)
    abs = error "The absolute value/magnitude of a dual number is currently undefined."
    signum = (signum <$>)
    fromInteger = toDual . fromInteger

conjugateDual :: Num a => Dual a -> Dual a
conjugateDual (Dual a b) = Dual a (-b)

instance (Eq a, Fractional a) => Fractional (Dual a) where 
    Dual 0 b / Dual 0 d = Dual (b / d) 0
    Dual _ _ / Dual 0 _ = error "Division of the form (a + bε) / (dε) where a is non-zero is undefined."
    Dual a b / Dual c d = Dual (a / c) ((b * c - a * d) / (c * c))
    fromRational = toDual . fromRational

instance (Eq a, Floating a) => Floating (Dual a) where
    -- TODO: A better definition of a ** b
    _ ** _ = error "Exponentiation of dual numbers is currently undefined."
    exp (Dual a b) = let scalar = exp a in Dual scalar (scalar * b)
    log (Dual a b) = Dual (log a) (b / a)
    pi = toDual pi
    -- https://en.wikipedia.org/wiki/Hyperbolic_functions
    sinh z = (exp z - exp (-z)) / 2
    cosh z = 0.5 * (exp z + exp (-z))
    tanh z = (exp z - exp (-z)) / (exp z + exp (-z))

derive :: (Eq a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Dual a)
derive f x = (f (x + epsilon) - f x) / epsilon

_real :: Lens (Dual a) (Dual a) a a
_real = lens real (\(Dual _ b) c -> Dual c b)

_nonreal :: Lens (Dual a) (Dual a) a a
_nonreal = lens nonreal (\(Dual a _) d -> Dual a d)