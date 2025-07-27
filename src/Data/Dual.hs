module Data.Dual where
import Control.Lens

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

-- | Epsilon (such that ε^2 = 0 but ε /= 0).
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
-- derive f x = (f (x + epsilon) - f x) / epsilon
-- @
derive :: (Ord a, Fractional a) => (Dual a -> Dual a) -> (Dual a -> Dual a)
derive f x = (f (x + epsilon) - f x) / epsilon

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

-- | The lens version of 'realFlatten'
realFlattened :: Num a => Getter (Dual a) (Dual a)
realFlattened = to realFlatten

-- | The lens version of 'nonrealFlatten'
nonrealFlattened :: Num a => Getter (Dual a) (Dual a)
nonrealFlattened = to nonrealFlatten

-- | Lens version of 'transposeDual'
transposed :: Getter (Dual a) (Dual a)
transposed = to transposeDual