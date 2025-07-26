module Data.Dual where
import Control.Lens

data Dual a = Dual { real :: a, nonreal :: a } deriving Eq

toDual :: Num a => a -> Dual a
toDual x = Dual x 0

dualOne :: Num a => Dual a
dualOne = Dual 1 0

epsilon :: Num a => Dual a
epsilon = Dual 0 1

instance (Ord a, Num a) => Ord (Dual a) where
    compare a b
        | difference == 0 = EQ
        | c == 0 = compare d 0
        | otherwise = compare c 0
        where difference@(Dual c d) = a - b

instance (Eq a, Num a, Show a) => Show (Dual a) where
    showsPrec context (Dual a 0) = showsPrec context a
    showsPrec context (Dual 0 b) = showParen (context > 7) $ showsPrec 7 b . showChar 'ε'
    showsPrec context z = showParen (context > 6) $ showsPrec 6 (z { nonreal = 0 }) . showString " + " . showsPrec 7 (z { real = 0 })

instance Functor Dual where
    fmap f (Dual a b) = Dual (f a) (f b)

-- Identical to Complex
instance Applicative Dual where
    (Dual f g) <*> (Dual a b) = Dual (f a) (g b)
    pure x = Dual x x

-- Also identical to Complex
instance Monad Dual where
    (Dual a b) >>= f = Dual (real $ f a) (nonreal $ f b)

instance (Ord a, Num a) => Num (Dual a) where
    (Dual a b) + (Dual c d) = Dual (a + c) (b + d)
    (Dual a b) * (Dual c d) = Dual (a * c) (a * d + b * c)
    negate = (negate <$>)
    abs x = if x >= 0 then x else -x
    signum = (signum <$>)
    fromInteger = toDual . fromInteger

conjugateDual :: Num a => Dual a -> Dual a
conjugateDual (Dual a b) = Dual a (-b)

instance (Ord a, Fractional a) => Fractional (Dual a) where 
    Dual 0 b / Dual 0 d = Dual (b / d) 0
    Dual _ _ / Dual 0 _ = error "Division of the form (a + bε) / (dε) where a is non-zero is undefined."
    Dual a b / Dual c d = Dual (a / c) ((b * c - a * d) / (c * c))
    fromRational = toDual . fromRational

instance (Ord a, Floating a) => Floating (Dual a) where
    -- TODO: A better definition of a ** b
    _ ** _ = error "Exponentiation of dual numbers is currently undefined."
    exp (Dual a b) = let scalar = exp a in Dual scalar (scalar * b)
    log (Dual a b) = Dual (log a) (b / a)
    pi = toDual pi
    -- https://en.wikipedia.org/wiki/Hyperbolic_functions
    sinh z = (exp z - exp (-z)) / 2
    cosh z = 0.5 * (exp z + exp (-z))
    tanh z = (exp z - exp (-z)) / (exp z + exp (-z))

rightSidedLimit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Dual a
rightSidedLimit target function = toDual . real $ function (target + epsilon)

leftSidedLimit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Dual a
leftSidedLimit target function = toDual . real $ function (target - epsilon)

limit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Maybe (Dual a)
limit target function = if left == right then Just left else Nothing where
    left = leftSidedLimit target function
    right = rightSidedLimit target function

derive :: (Ord a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Dual a)
derive f x = (f (x + epsilon) - f x) / epsilon

_real :: Lens (Dual a) (Dual a) a a
_real = lens real (\(Dual _ b) c -> Dual c b)

_nonreal :: Lens (Dual a) (Dual a) a a
_nonreal = lens nonreal (\(Dual a _) d -> Dual a d)

_transpose :: Getter (Dual a) (Dual a)
_transpose  = to (\(Dual a b) -> Dual b a)