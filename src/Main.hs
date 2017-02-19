{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List.Extra
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe
import           Data.String
import           Numeric.Natural
import           Test.QuickCheck
import           Text.PrettyPrint.Boxes

main :: IO ()
main = do
  quickCheck prop
  quickCheck valid

data V = A | P
  deriving (Show, Eq)

cleverIndices :: Natural -> Natural -> [[Natural]]
cleverIndices l s = cycle (cleverSegment l s)

-- The repeating patterns of interference
cleverSegment :: Natural -> Natural -> [[Natural]]
cleverSegment l s = filter (< l) <$> offsets l s

-- | The finite list of interference offsets
offsets :: Natural -> Natural -> [[Natural]]
offsets l s = genericTake (lcm l (s + 1) `div` l) inf
  where
    inf = fromState <$> states
    states = iterate advance initialState
    initialState :: Integer
    initialState = s'
    fromState :: Integer -> [Natural]
    fromState o =
      let n = 1 + ((l' - o) `div` (s' + 1))
      in fromInteger <$> genericTake n (iterate (+(s'+1)) o)
    advance :: Integer -> Integer
    advance o = (o - l') `mod` (s' + 1)
    l' = toInteger l
    s' = toInteger s

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
input s = cycle (genericReplicate s A ++ [P])

--------------------------------------------------------------------------------
-- test and utils
--------------------------------------------------------------------------------

afilter :: Alternative f => (a -> Bool) -> a -> f a
afilter c x = if c x
                then pure x
                else empty

prop :: Positive Natural -> Natural -> Property
prop (Positive l) s =
  let iters = 100
      checkLength = lcm l (s + 1) * iters
      expected = genericTake checkLength (bruteIndices l s)
      calculated = genericTake checkLength (cleverIndices l s)
  in expected === calculated

valid :: Positive Natural -> Natural -> Bool
valid (Positive l) s =
  let iters = 100
      checkLength = lcm l (s + 1) * iters
      calculated = genericTake checkLength (cleverIndices l s)
  in all (< l) (join calculated)

(.:) = (.).(.)

offsetLengthTable = makeTable (show .: f) [1..10] [1..11]
  where
    f x y = length $ offsets x (y-1)

makeTable :: forall a b. Show a => Show b => (a -> b -> String) -> [a] -> [b] -> Box
makeTable f xs ys = firstCol <+> hsep 1 top (col <$> xs)
  where
    firstCol :: Box
    firstCol = emptyBox 1 1 /+/ vcat left (fromString . show <$> ys) <+> emptyBox 1 1
    col :: a -> Box
    col x =
      let firstBox = fromString (show x)
      in firstBox /+/ vcat left ((fromString . f x) <$> ys)
