{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Parenthesize unary negation" #-}
module Data.Dual where
import Control.Lens
import Control.Monad.Trans.State
import Data.Maybe

-- | The main data type.
data Dual a = Dual { real :: a, nonreal :: a } deriving Eq

-- | Converts a number into 'Dual'.
--
-- @
-- toDual x = 'Dual' x 0
-- @
toDual :: Num a => a -> Dual a
toDual x = Dual x 0


-- | One.
--
-- @
-- dualOne = Dual 1 0
-- @
dualOne :: Num a => Dual a
dualOne = Dual 1 0

-- | Epsilon (such that ε ^ 2 = 0 but ε /= 0).
--
-- @
-- epsilon = Dual 0 1
-- @
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
    showsPrec context (Dual 0 b) = showParen (context > 7) $ showsPrec 7 b . showString " * " . showChar 'ε'
    showsPrec context z = showParen (context > 6) $ showsPrec 6 (realFlatten z) . showString " + " . showsPrec 7 (nonrealFlatten z)

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

instance (Ord a, Fractional a) => Fractional (Dual a) where 
    Dual 0 b / Dual 0 d = Dual (b / d) 0
    Dual _ _ / Dual 0 _ = error "Division of the form (a + bε) / (dε) where a is non-zero is undefined."
    Dual a b / Dual c d = Dual (a / c) ((b * c - a * d) / (c * c))
    fromRational = toDual . fromRational

-- | Takes a real-valued function and turns it into a dual-valued one.
convertFunctionToDual :: (Eq a, Num a) => (a -> a) -> (a -> a) -> (Dual a -> Dual a)
convertFunctionToDual f _ (Dual a 0) = toDual $ f a
convertFunctionToDual f f' (Dual a b) = Dual (f a) (b * f' a)

instance (Ord a, Floating a) => Floating (Dual a) where
    -- TODO: A better definition of a ** b
    Dual a b ** Dual c 0 = Dual (a ** c) (c * a ** (c - 1) * b)
    Dual a 0 ** Dual c d = let aToTheC = a ** c in Dual aToTheC (log a * aToTheC * d)
    a ** b = exp (log a * b)
    exp = convertFunctionToDual exp exp
    log = convertFunctionToDual log recip
    pi = toDual pi
    sin = convertFunctionToDual sin cos
    cos = convertFunctionToDual cos (negate . sin)
    -- https://en.wikipedia.org/wiki/Trigonometric_functions#Derivatives_and_antiderivatives
    tan = convertFunctionToDual tan ((\x -> x * x) . recip . tan)
    -- https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
    asin = convertFunctionToDual asin (\x -> 1 / sqrt (1 - x * x))
    acos = convertFunctionToDual acos (\x -> -1 / sqrt (1 - x * x))
    atan = convertFunctionToDual atan (\x -> 1 / sqrt (1 + x * x))
    -- https://en.wikipedia.org/wiki/Hyperbolic_functions
    sinh z = (exp z - exp (-z)) / 2
    cosh z = (exp z + exp (-z)) / 2
    tanh z = (exp z - exp (-z)) / (exp z + exp (-z))
    asinh z = log (z + sqrt (z * z + 1))
    acosh z = log (z + sqrt (z * z - 1))
    atanh z = log ((1 + z) / (1 - z)) / 2

-- https://en.wikipedia.org/wiki/Newton%27s_method
-- | Iterates Newton's method once.
newton :: (Ord a, Fractional a, Monad m) => (Dual a -> Dual a) -> StateT (Dual a) m (Dual a)
newton f = do
    modify \x -> x - (f x / fromJust (derive f x))
    get

-- | Conjugate.
--
-- @
-- conjugateDual (Dual a b) = Dual a (-b)
-- @
conjugateDual :: Num a => Dual a -> Dual a
conjugateDual (Dual a b) = Dual a (-b)

-- | Takes the right sided limit given a limiting value and a function.
--
-- @
-- rightSidedLimit target function = realFlatten $ function (target + epsilon)
-- @
rightSidedLimit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Dual a
rightSidedLimit target function = realFlatten $ function (target + epsilon)

-- | Takes the left sided limit given a limiting value and a function.
--
-- @
-- leftSidedLimit target function = realFlatten $ function (target - epsilon)
-- @
leftSidedLimit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Dual a
leftSidedLimit target function = realFlatten $ function (target - epsilon)

-- | Takes the limit given a limiting value and a function.
--
-- @
--  limit target function = if left == right then Just left else Nothing where
--      left = leftSidedLimit target function
--      right = rightSidedLimit target function
-- @
limit :: (Ord a, Num a) => Dual a -> (Dual a -> Dual a) -> Maybe (Dual a)
limit target function = if left == right then Just left else Nothing where
    left = leftSidedLimit target function
    right = rightSidedLimit target function

-- | Takes the derivative of a function.
--
-- @
-- derive f = flip limit (\x -> (f (x + epsilon) - f x) / epsilon)
-- @
derive :: (Ord a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Maybe (Dual a))
derive f = flip limit (\x -> (f (x + epsilon) - f x) / epsilon)

-- | Sets the dual part to zero.
--
-- @
-- realFlatten (Dual a _) = Dual a 0
-- @
realFlatten :: Num a => Dual a -> Dual a
realFlatten (Dual a _) = Dual a 0 

-- | Sets the real part to zero.
--
-- @
-- flattenReal (Dual a _) = Dual a 0 
-- @
nonrealFlatten :: Num a => Dual a -> Dual a
nonrealFlatten (Dual _ b) = Dual 0 b

-- | Swaps the components of a dual number.
--
-- @
-- transposeDual (Dual a b) = Dual b a
-- @
transposeDual :: Dual a -> Dual a
transposeDual (Dual a b) = Dual b a

-- | A lens for getting the real component of a dual number.
_real :: Lens (Dual a) (Dual a) a a
_real = lens real (\(Dual _ b) c -> Dual c b)

-- | A lens for getting the nonreal component of a dual number.
_nonreal :: Lens (Dual a) (Dual a) a a
_nonreal = lens nonreal (\(Dual a _) d -> Dual a d)

-- | Lens version of 'conjugate'.
conjugated :: Num a => Getter (Dual a) (Dual a)
conjugated = to conjugateDual

-- | The lens version of 'realFlatten'
realFlattened :: Num a => Getter (Dual a) (Dual a)
realFlattened = to realFlatten

-- | The lens version of 'nonrealFlatten'
nonrealFlattened :: Num a => Getter (Dual a) (Dual a)
nonrealFlattened = to nonrealFlatten

-- | Lens version of 'transposeDual'
transposed :: Getter (Dual a) (Dual a)
transposed = to transposeDual