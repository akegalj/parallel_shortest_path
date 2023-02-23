{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Parallel.Strategies
import qualified Data.ByteString             as B
import           Data.Sequence               (Seq (..), (|>))
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Exts                    (fromList, toList)
import           Data.Compact

import           Control.Monad.ST
import           Data.Word

type Node = Word16
data Graph = Graph
    { adjs :: {-# UNPACK #-} !(UV.Vector Node)
    , offs :: {-# UNPACK #-} !(UV.Vector Word32)
    , lens :: {-# UNPACK #-} !(UV.Vector Word16)
    }

data MGraph s = MGraph
    { _adjs   :: {-# UNPACK #-} !(MV.MVector s Node)
    , _offs   :: {-# UNPACK #-} !(MV.MVector s Word32)
    , _lens   :: {-# UNPACK #-} !(MV.MVector s Word16)
    , adjsInd :: {-# UNPACK #-} !Int
    }

type Row = Int
type Col = Node

type Queue = Seq Node
type Distances s = MV.MVector s Word16

neighbours :: Graph -> Node -> UV.Vector Node
neighbours (Graph adjs offs lens) n' = GV.unsafeSlice (fromIntegral $ offs `GV.unsafeIndex` n) (fromIntegral $ lens `GV.unsafeIndex` n) adjs
  where
    n = fromIntegral n'

parse :: B.ByteString -> Graph
parse bs = runST $ do
    let nc = fromIntegral nodeCount
    adjsInit <- MV.unsafeNew $ nc*nc
    offsInit <- MV.unsafeNew nc
    lensInit <- MV.unsafeNew nc
    let gf = MGraph adjsInit offsInit lensInit 0

    gf2 <- parseFile gf 0

    adjsFin <- GV.unsafeFreeze $ _adjs gf2
    offsFin <- GV.unsafeFreeze $ _offs gf2
    lensFin <- GV.unsafeFreeze $ _lens gf2

    pure $ sliceAdjs $ Graph adjsFin offsFin lensFin

  where
    sliceAdjs gf = gf { adjs = GV.unsafeSlice 0 (GV.foldl' (\ac i -> ac + fromIntegral i) 0 $ lens gf) $ adjs gf }
    bsLen = B.length bs
    nodeCount = truncate . sqrt $ (fromIntegral bsLen + 1) / 2
    lineLen = fromIntegral $ nodeCount*2

    parseLine :: MGraph s -> Row -> Col -> Node -> Int -> ST s (MGraph s)
    parseLine gf !row !col !count !ind
        | col == nodeCount = do
            if row == 0
                then MV.unsafeWrite (_offs gf) row 0
                else do
                    let prevRow = row - 1
                    old <- MV.unsafeRead (_offs gf) prevRow
                    oldCount <- MV.unsafeRead (_lens gf) prevRow
                    MV.unsafeWrite (_offs gf) row (old + fromIntegral oldCount)
            MV.unsafeWrite (_lens gf) row count
            pure gf
        | otherwise = do
                let c = B.index bs ind
                if c == 49 -- '1'
                    then do
                        let newCount = count + 1
                        MV.unsafeWrite (_adjs gf) (adjsInd gf) col
                        parseLine (gf { adjsInd = adjsInd gf + 1 }) row (col + 1) newCount (ind + 2)
                    else parseLine gf row (col + 1) count (ind + 2)

    parseFile :: MGraph s -> Row -> ST s (MGraph s)
    parseFile gf !row
        | row == fromIntegral nodeCount = pure gf
        | otherwise = do
                gf2 <- parseLine gf row 0 0 $ row * lineLen
                parseFile gf2 (row + 1)

distanceNode :: Node -> Graph -> Word32
distanceNode n gf = runST $ do
    d <- MV.replicate l 0
    UV.foldM' (\q _ -> go q d) [n] $ UV.enumFromTo 1 l
    MV.foldl' (\acc i -> acc + fromIntegral i) 0 d
  where
    l = GV.length $ offs gf

    go :: Queue -> Distances s -> ST s Queue
    go (x :<| xs) dist = do
        step <- (+1) <$> MV.unsafeRead dist (fromIntegral x)
        next <- GV.foldM' (\xs' i' -> do
            let i = fromIntegral i'
            s <- MV.unsafeRead dist i
            if s == 0
                then do
                    MV.unsafeWrite dist i step
                    pure $ xs' |> i'
                else pure xs'
            ) xs $ neighbours gf x
        pure next

distance :: Graph -> Word32
distance g = sum . withStrategy (parList rpar) . zipWith distanceNode [0..] $ replicate (GV.length $ offs g) g

main = B.getContents >>= compact . parse >>= print . distance . getCompact