{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moire
  ( moire
  ) where

import           Data.List.Extra
import           Numeric.Natural

-- The repeating patterns of interference
moire
  :: Natural
  -- ^ The size of each window
  -> Natural
  -- ^ The distance between each painted space
  -> [[Natural]]
  -- ^ The indices in each window which are painted.
moire l s =
  let numWindows = (lcm l s `div` l)
      painted = iterate (+s) 0
      inf = (`divMod` l) <$> painted
  in genericTake numWindows (unindices inf)

unindices :: [(Natural, a)] -> [[a]]
unindices = go 0
  where
    go _ [] = []
    go n xs = let (is, js) = break ((> n) . fst) xs
              in (snd <$> is) : go (n+1) js
