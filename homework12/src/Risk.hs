{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad.Random
import Control.Monad
import Data.List (sortBy)
import Data.Monoid (mempty)


------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Monoid DieValue where
  mempty = DV 0
  mappend (DV x) (DV y) = DV (x + y)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

zipPad :: (Monoid a, Monoid b) => [a] -> [b] -> [(a,b)]
zipPad xs ys = take maxLength $ zip (pad xs) (pad ys)
    where
        maxLength = max (length xs) (length ys)
        pad v = v ++ repeat mempty

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do attDice <- dice (attUnits bf)
               dffDice <- dice (dffUnits bf)
               return (update bf (compareDice (packDice attDice dffDice)))


attUnits :: Battlefield -> Army
attUnits (Battlefield atts _) = max 3 (min 0 (atts-1))

dffUnits :: Battlefield -> Army
dffUnits (Battlefield _ dffs) = max 2 dffs

update :: Battlefield -> (Army, Army) -> Battlefield
update (Battlefield atts dffs) (a, d) = Battlefield (atts-d) (dffs-a)

compareDice :: [(DieValue, DieValue)] -> (Army, Army)
compareDice xs = (attWins, dffWins)
                  where
                    dffWins = length (filter (<0) comparison)
                    attWins = length (filter (>0) comparison)
                    comparison = uncurry (-) <$> xs

packDice :: [DieValue] -> [DieValue] -> [(DieValue, DieValue)]
packDice attDice dffDice = zipPad (sortBy (flip compare) attDice) (sortBy (flip compare)dffDice)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield atts dffs)
            | atts < 2 || dffs <= 0 = return bf
            | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= success

success :: [Battlefield] -> Rand StdGen Double
success bfs = return (fromIntegral (countWins bfs) / fromIntegral (length bfs))

countWins :: [Battlefield] -> Int
countWins bfs = length $ filter (==True) (attackerWins <$> bfs)

attackerWins :: Battlefield -> Bool
attackerWins (Battlefield atts dffs) = dffs <= 0
