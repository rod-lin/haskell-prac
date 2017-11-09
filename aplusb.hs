{-# LANGUAGE GADTs, TypeFamilies, TypeOperators #-}

module Kata.AdditionCommutes (plusCommutes) where

-- import Kata.AdditionCommutes.Definitions
--     (Z, S, Natural(..), Equal(..), (:+:))

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:

-- | The natural numbers, encoded in types.
data Z         -- zero
data S n       -- succ

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS a) = EqlS (symmetric a)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS a) (EqlS b) = EqlS (transitive a b)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b') = EqlS (plusCommutes NumZ b')
plusCommutes (NumS a') NumZ = EqlS (plusCommutes a' NumZ)
plusCommutes a@(NumS a') b@(NumS b') = let
        lem1 = EqlS (plusCommutes a' b')
        lem2 = plusCommutes a b'
        lem3 = plusCommutes b a'
    in symmetric (EqlS (transitive (symmetric lem2) (transitive lem1 lem3)))
