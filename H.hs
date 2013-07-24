{-# LANGUAGE BangPatterns #-}
import System.Environment 
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random.MWC

type Pos = (Int,Int)
type RandInts = U.Vector Int
type TileReplacer = (Int, Tile)

data Tile = Tile
    { tPos  :: !Pos
    , tType :: !Int
    } deriving (Show)

data Room = Room
    { rPos   :: !Pos
    , rw, rh :: !Int
    } deriving (Show) 

data Lev = Lev
    { lRs :: !(V.Vector Room)
    , lTs :: !(V.Vector Tile)
    } 

levDim, minWid, maxWid :: Int
levDim = 50
minWid = 2
maxWid = 8

genRooms :: Int -> RandInts ->V.Vector Room -> (V.Vector Room,RandInts)
genRooms 0  rands done = (done,rands)
genRooms !n randInts rsDone =
    if checkBound tr
    then noFit
    else if checkColl tr rsDone
         then noFit  
         else genRooms (n-1) (restInts) (V.cons tr rsDone) 
  where
    noFit = genRooms (n-1) (restInts) rsDone 
    tr = Room {rPos=(x,y), rw= w, rh= h}
    x = rem (U.unsafeHead randInts) levDim
    y = rem (U.unsafeIndex randInts 2) levDim
    restInts = U.unsafeDrop 2 randInts
    w = rem (U.unsafeHead randInts) maxWid + minWid 
    h = rem (U.unsafeIndex randInts 2) maxWid + minWid

checkBound :: Room -> Bool
checkBound Room{rPos=(x,y),rw=w,rh=h} =
    x<=0 || y<=0 || x+w >= levDim || y+h >= levDim

checkColl :: Room -> V.Vector Room -> Bool
checkColl r vr =
    if isNull == False
    then if roomHitRoom r x
         then True
         else checkColl r xs
    else False
  where 
    x = V.unsafeHead vr
    xs = V.unsafeTail vr 
    isNull = V.null vr

roomHitRoom :: Room -> Room -> Bool
roomHitRoom Room {rPos=(x,y), rw= w, rh= h} Room {rPos=( x2, y2), rw= w2, rh= h2} 
    = not ((x2+w2+1) < x || x2 > (x+w+1)
        || (y2+h2+1) < y || y2 > (y+h+1))

genTiles' :: Int -> Int-> V.Vector Tile -> V.Vector Tile
genTiles' 0 _ done = done 
genTiles' nleft n ts = genTiles' (nleft-1) (n+1) (V.cons Tile{tPos=pPos, tType=(0)} ts)
  where
    pPos = case quotRem n levDim of (x,y) -> (y,x)

rs2Ps :: V.Vector Room -> V.Vector TileReplacer
rs2Ps vr  =
    if isNull
    then V.empty
    else (room2Replacers r (w*h) V.empty) V.++ rs2Ps rs
  where
    r@Room{rPos = (_,_), rw = w, rh = h} = V.unsafeHead vr
    rs = V.unsafeTail vr
    isNull = V.null vr  

room2Replacers :: Room -> Int -> V.Vector TileReplacer -> V.Vector TileReplacer
room2Replacers _ 0 done = done
room2Replacers (thisR@Room{rPos = (x,y), rw = w, rh = h}) n ps =   (V.cons currentPoint ps) V.++  (room2Replacers thisR (n-1) ps)
  where
    currentPoint = point2Replacer(x+xofs,y+yofs)  
    num = w*h - n
    xofs = rem num w
    yofs = quot num w    

point2Replacer :: Pos -> TileReplacer
point2Replacer (x,y) = (y*levDim + x, Tile{tPos=(x,y), tType=1}) 

room2Tiles :: V.Vector Room -> V.Vector Tile -> V.Vector Tile
room2Tiles vr vt = V.unsafeUpdate vt (rs2Ps vr)  

showTiles' :: V.Vector Tile -> String -> String
showTiles' vt s =
    if V.null vt
    then s
    else s ++ (case t of 0 -> '0'; 1 -> '1'; _ -> ' '):break ++ showTiles' xs s
  where
    break = if rem x levDim == 0 then "\n" else ""
    Tile{tPos=(x,_), tType=t} = V.unsafeHead vt
    xs = V.unsafeTail vt

showLev :: Lev -> String
showLev Lev{lRs = _, lTs = ts} = showTiles' ts []

genLevs' :: Int -> V.Vector Lev -> RandInts -> (V.Vector Lev,RandInts) 
genLevs' 0 done rands = (done,rands) 
genLevs' n ldone (randInts) = do
    let (rs,rands) = genRooms 50000 (randInts) V.empty
    let tstemp = genTiles' (levDim*levDim) 0 V.empty
    let ts = room2Tiles rs tstemp
    genLevs' (n-1) ( V.cons Lev{lRs = rs, lTs = ts} ldone) rands

biggestLev :: V.Vector Lev -> Int -> Int -> Int
biggestLev v maxval maxnum =
    if isNull
    then maxnum
    else if len > maxval
         then biggestLev ls len thisNum
         else biggestLev ls maxval maxnum 
  where
    Lev{lRs = rs, lTs = _} = V.unsafeHead v
    len = V.length rs
    thisNum = V.length v
    ls = V.unsafeTail v
    isNull = V.null v

main :: IO ()
main = do 
    (v:_) <- getArgs
    putStr "The random seed is: " 
    putStrLn v
    withSystemRandom . asGenIO $ \ gen -> do
        rands <- uniformVector gen 10000000
        let (ls, _) = genLevs' 100 V.empty rands
            levNum  = biggestLev ls 0 0
            levStr  = showLev $ V.unsafeIndex ls levNum
        putStr levStr
