{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List.Extra
import           Data.String
import           Numeric.Natural
import           Test.QuickCheck
import           Text.PrettyPrint.Boxes

import           Moire

main :: IO ()
main = do
  quickCheck prop
  quickCheck valid

data V = A | P
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- brute forcing
--------------------------------------------------------------------------------

brute :: Natural -> Natural -> [[V]]
brute l s =
  let inp = input s
      out = repeat P
      f A x = x
      f P _ = A
  in chunksOf (fromIntegral l) (zipWith f inp out)

bruteIndices :: Natural -> Natural -> [[Natural]]
bruteIndices l s =
  let b = brute l s
      toI = fmap fromIntegral . elemIndices A
  in toI <$> b

input :: Natural -> [V]
input s = cycle (P : genericReplicate (s - 1) A)

--------------------------------------------------------------------------------
-- test and utils
--------------------------------------------------------------------------------

afilter :: Alternative f => (a -> Bool) -> a -> f a
afilter c x = if c x
                then pure x
                else empty

prop :: Positive Natural -> Positive Natural -> Property
prop (Positive l) (Positive s) =
  let iters = 100
      checkLength = lcm l s * iters
      expected = genericTake checkLength (bruteIndices l s)
      calculated = genericTake checkLength (cycle (moire l s))
  in expected === calculated

valid :: Positive Natural -> Positive Natural -> Bool
valid (Positive l) (Positive s) =
  let iters = 100
      checkLength = lcm l s * iters
      calculated = genericTake checkLength (cycle (moire l s))
  in all (< l) (join calculated)

(.:) = (.).(.)

makeTable
  :: forall a b
   . (Show a, Show b)
  => (a -> b -> String) -> [a] -> [b] -> Box
makeTable f xs ys = firstCol <+> hsep 1 top (col <$> xs)
  where
    firstCol :: Box
    firstCol = emptyBox 1 1 /+/ vcat left (fromString . show <$> ys) <+> emptyBox 1 1
    col :: a -> Box
    col x =
      let firstBox = fromString (show x)
      in firstBox /+/ vcat left ((fromString . f x) <$> ys)
