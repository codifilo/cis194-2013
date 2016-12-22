{-# OPTIONS_GHC -Wall #-}
module Lib where

import Employee
import Data.Tree

-- Exercise 1 -----------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons emp@Emp{empFun = ef} (GL lst gf) = GL (emp:lst) (ef+gf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b


-- Exercise 2 -----------------------------------------
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init Node{rootLabel = rl, subForest = sf} = f rl (map (treeFold f init) sf)

-- | First part of list is with boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        -- ^ The new withoutBossList has sub bosses in it.

        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss
        -- ^ The new withBossList doesn't have sub bosses in it.

maximumS ::(Monoid a, Ord a) => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max res
  where res = treeFold nextLevel (mempty, mempty) tree

parse :: GuestList -> String
parse (GL empl0 fun) = "Fun score : "
                    ++ show fun ++ "\n"
                    ++ unlines (map empName empl0)
