{-# LANGUAGE DeriveFunctor
  , GeneralizedNewtypeDeriving
  , TypeFamilies
  #-}

module Main where

import Control.Comonad.Cofree
import Control.Lens
import Control.Monad (guard)
import Data.Functor.Foldable
import Data.Maybe (isJust)
import Data.Monoid.Inf
  ( Inf(Finite, Infinity)
  , NegInf
  )
import qualified Data.Monoid.Inf as Inf
import Silly

type Value = NegInf Double

clamp :: NegInf Double -> Double
clamp x = case x of
              Infinity -> 0
              Finite k -> k

maximum_ :: [Double] -> Double
maximum_ = clamp . Inf.maximum

knapsack_1 :: [(Int, Double)] -> Int -> Double
knapsack_1 weights c = maximum_ candidates
  where
    candidates = do
        (w, v) <- weights
        let x = knapsack_1 weights $ c - w
        guard $ (0 < w) && (w <= c)
        return $ v + x

newtype I = I Int deriving (Eq, Show, Num, Enum)
data Nat a = S a | Z deriving (Eq, Show, Functor)

type instance Base I = Nat

instance Foldable I where
    project (I 0) = Z
    project x = S $ pred x

knapsack :: [(I, Double)] -> I -> Double
knapsack weights = histo f
   where
    f :: Nat (Cofree Nat Double) -> Double
    f x = case x of
              Z   -> 0
              S t -> maximum_ $ do
                        (w', v) <- weights
                        let w = w' - 1
                            mu@(~(Just u)) = lookback t w
                        guard $ isJust mu
                        return $ v + u

    lookback :: Cofree Nat Double -> I -> Maybe Double
    lookback (k :< _) 0 = Just k
    lookback (_ :< Z) _ = Nothing
    lookback (_ :< (S inner)) n = lookback inner (n - 1)

main :: IO ()
main = let wvs = [(12,4),(1,2),(2,2),(1,1),(4,10)]
           cap = 15
           sol1 = knapsack_1 wvs cap
           sol2 = knapsack (wvs & traverse . _1 %~ I ) (I cap)
           sth = silly (T 15)
        in print sol1 >> print sol2 >> print sth
