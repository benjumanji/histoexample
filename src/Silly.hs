{-# LANGUAGE BangPatterns
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , TypeFamilies #-}

module Silly where

import Control.Comonad.Cofree
import Data.Functor.Foldable

newtype T = T Int deriving (Eq, Show, Num, Enum, Integral, Real, Ord)

data Tree a = Bin Int a a | Tip Int | Empty deriving (Eq, Ord, Functor)

type instance Base T = Tree

instance Foldable T where
    project (T 0) = Empty
    project (T 1) = Tip
    project n = Bin low high
      where
        (q,r) = n `divMod` 2
        low = q + r
        high = q

count :: Tree (Cofree Tree Int) -> Int
count Empty = 0
count Tip   = 1
count (Bin (l :< _) (r :< _)) = l + r

silly :: T -> Int
silly = histo count

{-
search :: Int -> Int -> Cofree Tree Double -> Maybe Double
search
search size needle (_ :< (Bin l r)) | low <= needle = search low needle l
                                    | otherwise     = search high $ needle - low
  where
    (q,r) = size `div` 2
    low = q + r
    high = q
-}
