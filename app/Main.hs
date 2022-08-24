{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Parallel.Strategies
import           Data.IntSet                 (IntSet, size, union, unions, (\\))
import           Data.Sequence               (Seq (..), (|>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Data.Vector                 (Vector, (!))
import           GHC.Exts                    (fromList, toList)

type Node = Int
type Graph = Vector IntSet
type Queue = Seq IntSet
type Visited = IntSet

parse :: T.Text -> Graph
parse = fromList . map (fromList . toNodes . T.split (==' ')) . T.lines
  where
    toNodes = map fst . filter ((=="1") . snd) . zip [0..]

distanceNode :: Node -> Graph -> Int
distanceNode n gf = go [[n]] [n] 1 0
  where
    append a [] = a
    append a b  = a |> b

    neighbours = unions . map (gf!) . toList

    go :: Queue -> Visited -> Int -> Int -> Int
    go Empty      _  _     !res = res
    go (x :<| xs) vs !step !res = let next = neighbours x \\ vs
                                  in go (append xs next) (union vs next) (step + 1) $ res + step*(size next)

distance :: Graph -> Int
distance g = sum . parallel . zipWith distanceNode [0..] $ replicate (length g) g

parallel = withStrategy (parList rpar)

main = T.getContents >>= print . distance . parse