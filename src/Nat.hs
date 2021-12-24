module Nat where

data Nat = Zero | Next Nat
    deriving (Eq, Show, Ord)

instance Num Nat where
    (+) a Zero = a
    (+) a (Next b) = Next $ a + b 
    (*) a Zero = Zero
    (*) a (Next b) = a + a * b
    negate _ = error "Nat cannot be negative"
    abs n = n
    signum Zero = Zero
    signum _ = 1
    fromInteger 0 = Zero
    fromInteger n = Next $ fromInteger n

beside :: Nat -> Nat -> Bool
beside a b = a == Next b || b == Next a

beside2 :: Nat -> Nat -> Bool
beside2 a b = b - a < 2

