{-# LANGUAGE BangPatterns #-}

import Data.List.Split                  (chunksOf)
import System.Environment
import qualified Data.Vector.Unboxed    as U

import System.Random                    (RandomGen, next)
import Random.Xorshift


data Tile = Wall | Space

type Rooms = U.Vector (Int, Int, Int, Int)              -- x, y, w, h

levDim, minWid, maxWid :: Int
levDim = 50
minWid = 2
maxWid = 8


genRooms :: RandomGen g => Int -> g -> (Rooms, g)
genRooms i gen =
    go i gen U.empty
  where
    go 0 rands done = (done, rands)
    go !n randInts rsDone =
        if checkBound tr || checkColl tr rsDone
            then go (n-1) restInts rsDone
            else go (n-1) restInts (U.cons tr rsDone)
      where
        (r1, r)  = next randInts
        (r2, restInts) = next r
        tr       = (x, y, w, h)
        x        = rem r1 levDim
        y        = rem r2 levDim
        w        = rem r1 maxWid + minWid
        h        = rem r2 maxWid + minWid

checkBound (x, y, w, h) =
    x<=0 || y<=0 || x+w >= levDim || y+h >= levDim

checkColl room =
    U.any (roomHitRoom room)

roomHitRoom (x, y, w, h) (x2, y2, w2, h2)
    = not ((x2+w2+1) < x || x2 > (x+w+1)
        || (y2+h2+1) < y || y2 > (y+h+1))
    
inRoom (x, y) (rx, ry, rw, rh) =
        (rx <= x) && (x < rx + rw)
    &&  (ry <= y) && (y < ry + rh)


showTiles :: [Tile] -> String
showTiles =
    unlines . chunksOf levDim . map toChar
  where
    toChar t = case t of
        Wall  -> '0'
        Space -> '1'


genLevs' :: RandomGen g => Int -> g -> [Tile]
genLevs' i gen =
    go i gen (0, [])
  where
    go 0 _ (_, done)  = done
    go !n randInts current@(rooms, _) =
        go (n-1) rands next
      where
        (rs, rands) = genRooms 50000 randInts
        tiles       = map toTile [1 .. levDim ^ 2]
        next        = let rooms' = U.length rs in if rooms' > rooms then (rooms', tiles) else current
        toTile n    = let pos = toPos n in if U.any (pos `inRoom`) rs then Space else Wall
        toPos n     = let (y, x) = quotRem n levDim in (x, y)


main :: IO ()
main = do
    (v:_) <- getArgs
    putStrLn $ "The random seed is: " ++ v
    gen <- newXorshift
    let lTiles = genLevs' 100 gen
    putStr $ showTiles lTiles
