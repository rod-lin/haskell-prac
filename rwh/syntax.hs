{-# LANGUAGE ExistentialQuantification #-}

data B = forall a. Show a => O a

instance Show B where
	show (O v) = show v

a = [ O 1, O "hi" ]

val (O v) = v

