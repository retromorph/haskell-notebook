module Nat where

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Ord)

instance Num Nat where
    a + Zero = a
    Zero + a = a
    Succ a + Succ b = Succ . Succ $ a + b 
    a * Zero = Zero
    a * Succ b = a + a * b
    negate _ = error "Nat cannot be negative"
    abs n = n
    signum Zero = Zero
    signum _ = Succ Zero
    fromInteger 0 = Zero
    fromInteger n = Succ . fromInteger $ n - 1

beside :: Nat -> Nat -> Bool
beside Zero Zero = False
beside a (Succ b) = a == b
beside (Succ a) b = a == b

beside2 :: Nat -> Nat -> Bool
beside2 a b = beside a b || beside (Succ a) b || beside a (Succ b)

pow :: Nat -> Nat -> Nat
pow a Zero = 1
pow a (Succ b) = a * pow a b
