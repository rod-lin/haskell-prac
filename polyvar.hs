{-# LANGUAGE FlexibleInstances,
             TypeFamilies,
             FunctionalDependencies,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

module PolyvariadicFunctions where

class IntOrRest a where
    polyAdd' :: Int -> a

instance (a ~ Int, IntOrRest x) => IntOrRest (a -> x) where
    polyAdd' init next = polyAdd' (init + next)

instance IntOrRest Int where
    polyAdd' total = total

polyAdd :: IntOrRest a => a
polyAdd = polyAdd' 0

class AnyOrRest elem a | a -> elem where
    polyList' :: [ elem ] -> a

instance AnyOrRest elem a => AnyOrRest elem (elem -> a) where
    polyList' init next = polyList' (init ++ [next])

instance AnyOrRest elem [elem] where
    polyList' total = total

polyList :: AnyOrRest elem a => a
polyList = polyList' []

class StringOrRest a where
    polyWords' :: [String] -> a

instance (a ~ String, StringOrRest x) => StringOrRest (a -> x) where
    polyWords' init next = polyWords' (init ++ [next])

instance StringOrRest String where
    polyWords' total = unwords total

polyWords :: StringOrRest a => a
polyWords = polyWords' []
