{-# LANGUAGE FlexibleInstances #-}

data Curse a = S''t a

class TC a where
    foo :: a -> String

-- allowed without FlexibleInstances

instance TC (Curse Int) where
    foo a = "wt?"

data T = A Int | B Int deriving (Eq, Ord)

type Tuple3 = (,,)
type Kind3 a b c = a -> b -> c

f :: (Tuple3 String String String) -> String
f g = "string"
