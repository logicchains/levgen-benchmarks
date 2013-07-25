{-# LANGUAGE BangPatterns #-}
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random
import Random.Xorshift

type Pos = (Int,Int)
type RandInts = U.Vector Int

data Tile = Wall | Space deriving (Show)

data Room = Room
    { rPos :: !Pos
    , rw, rh :: !Int
    } deriving (Show)

data Lev = Lev
    { lRooms :: !(V.Vector Room)
    , lTiles :: [Tile]
    }

levDim, minWid, maxWid :: Int
levDim = 50
minWid = 2
maxWid = 8

genRooms :: Int -> RandInts -> V.Vector Room -> (V.Vector Room,RandInts)
genRooms 0 rands done = (done,rands)
genRooms !n randInts rsDone =
    if checkBound tr
    then noFit
    else if checkColl tr rsDone
         then noFit
         else genRooms (n-1) (restInts) (V.cons tr rsDone)
  where
    noFit    = genRooms (n-1) (restInts) rsDone
    tr       = Room {rPos=(x,y), rw= w, rh= h}
    x        = rem (U.unsafeHead randInts) levDim
    y        = rem (U.unsafeIndex randInts 2) levDim
    restInts = U.unsafeDrop 2 randInts
    w        = rem (U.unsafeHead randInts) maxWid + minWid
    h        = rem (U.unsafeIndex randInts 2) maxWid + minWid

checkBound :: Room -> Bool
checkBound Room { rPos = (x,y), rw = w, rh = h } =
    x<=0 || y<=0 || x+w >= levDim || y+h >= levDim

checkColl :: Room -> V.Vector Room -> Bool
checkColl room = V.any (roomHitRoom room)

roomHitRoom :: Room -> Room -> Bool
roomHitRoom Room {rPos=(x,y), rw= w, rh= h} Room {rPos=( x2, y2), rw= w2, rh= h2}
    = not ((x2+w2+1) < x || x2 > (x+w+1)
        || (y2+h2+1) < y || y2 > (y+h+1))

inRoom :: Pos -> Room -> Bool
inRoom (x, y) (Room (rx, ry) rw rh) =
        (rx <= x) && (x < rx + rw)
    &&  (ry <= y) && (y < ry + rh)

showTiles :: [Tile] -> String
showTiles = unlines . chunksOf levDim . map toChar
  where
    toChar t = case t of
        Wall  -> '0'
        Space -> '1'

genLevs' :: Int -> V.Vector Lev -> RandInts -> (V.Vector Lev,RandInts)
genLevs' 0 done rands = (done,rands)
genLevs' n ldone randInts =
    genLevs' (n-1) ( V.cons Lev{lRooms = rs, lTiles = tiles} ldone) rands
  where
    (rs,rands) = genRooms 50000 (randInts) V.empty
    toTile n = if (V.any (toPos n `inRoom`) rs) then Space else Wall
    tiles = map toTile [1 .. levDim ^ 2]
    toPos n = let (y, x) = quotRem n levDim in (x, y)

biggestLev :: V.Vector Lev -> Lev
biggestLev = V.maximumBy (comparing (V.length . lRooms))

main :: IO ()
main = do
    (v:_) <- getArgs
    putStr "The random seed is: "
    putStrLn v
    gen <- newXorshift
    let rands = U.unfoldrN 10000000 (Just . next) gen
    let (levs, _) = genLevs' 100 V.empty rands
    putStr $ showTiles $ lTiles $ biggestLev levs
