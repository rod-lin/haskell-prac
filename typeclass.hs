module TryTypeclacc where

class Cute a where
    speak :: a -> String

data Dog = BlackDog | WhiteDog deriving (Show)

instance Cute Dog where
    speak dog = "wu~~~"

data Cat = FlyingCat | CrawlingCat deriving (Show)

instance Cute Cat where
    speak cat = "meo~~~"
