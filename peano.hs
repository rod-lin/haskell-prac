import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano

one = Succ Zero

-- Addition
add n Zero = n
add a (Succ n) = add (Succ a) n

-- Subtraction
sub Zero Zero = Zero
sub Zero n = error "negative number"
sub n Zero = n
sub (Succ a) (Succ b) = sub a b

-- Multiplication
mul n Zero = Zero
mul Zero n = Zero
mul n (Succ Zero) = n
mul (Succ Zero) n = n
mul a b = add (mul a (sub b one)) a

-- Integer division
div n Zero = error "divide by 0"
div Zero n = Zero
div a b = if compare a b == LT then Zero else add one (div (sub a b) b)

even, odd :: Peano -> Bool
-- Even
even Zero = True
even n = odd (sub n one)

-- Odd
odd Zero = False
odd n = even (sub n one)

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ
compare n Zero = GT
compare Zero n = LT
compare a b = compare (sub a one) (sub b one)
