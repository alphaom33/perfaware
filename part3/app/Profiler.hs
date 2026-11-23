{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE CPP #-}

module Profiler where
import GHC.IO (unsafePerformIO, evaluate)
import Data.IORef (newIORef, modifyIORef', readIORef, IORef, writeIORef)
import Control.Arrow (Arrow(second, first))
import Data.Array.Base ((!?), thawIOArray)
import Data.Array (Array, array)
import qualified Data.Array.Base as A
import Data.Array.ST (writeArray)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map (empty, alter)
import qualified Data.Map as M
import Control.Monad (when)

import Time

type Anchors = Array Int (String, Float)

{-# NOINLINE (//) #-}
(//) :: Anchors -> [(Int, (String, Float))] -> Anchors
(//) a [(i, v)] = unsafePerformIO $ do
    arr <- thawIOArray a
    writeArray arr i v
    A.unsafeFreeze arr
(//) _ _ = error "why"

{-# NOINLINE times #-}
times :: IORef Anchors
times = unsafePerformIO $ newIORef $ array (0,8192) [(x, ("", 0)) | x <- [0..8192]]

{-# NOINLINE stackCount #-}
stackCount :: IORef Int
stackCount = unsafePerformIO $ newIORef 0

{-# NOINLINE size #-}
size :: IORef Int
size = unsafePerformIO $ newIORef 0

{-# NOINLINE startTime #-}
startTime :: IORef Float
startTime = unsafePerformIO $ newIORef 0

{-# NOINLINE divisor #-}
divisor :: IORef Float
divisor = unsafePerformIO $ newIORef 0

addTime :: Int -> String -> Float -> IO ()
addTime i name time = do
    modifyIORef' size (+1)
    modifyIORef' times (// [(i,(name,time))])

getTimesIndex :: Int -> IO (String, Float)
getTimesIndex i = do
    gotTimes <- readIORef times
    return $ fromJust $ gotTimes !? i

modifyTimesIndex :: Int -> (Float -> Float) -> IO ()
modifyTimesIndex i f = do
    gotTimes <- readIORef times
    modifyIORef' times (// [(i, second f $ fromJust $ gotTimes !? i)])

killStack :: Int -> Int -> Float -> IO ()
killStack i ci diff = do
    curr <- getTimesIndex (i - ci)
    if snd curr < 1 then do
        modifyTimesIndex (i - ci) (- diff)
        when (ci < -1) $ killStack i (ci + 1) diff
    else killStack (i - 1) ci diff

-- #ifdef PROFILE
{-# NOINLINE profileIO #-}
profileGeneric :: String -> (b -> IO a) -> b -> b
profileGeneric name op val = unsafePerformIO $ do
    count <- readIORef stackCount 
    i <- readIORef size
    gotDivisor <- readIORef divisor
    addTime i name 0

    beginTime <- getRdtscTime 1

    writeIORef stackCount (count + 1)
    _ <- op val
    writeIORef stackCount count

    endTime <- getRdtscTime 1

    let diff = fromIntegral (endTime - beginTime) / gotDivisor
    modifyTimesIndex i (+ diff)
    time <- getTimesIndex i 
    when (count > 0) $ killStack (i - count) -count (snd time)
    return val

{-# NOINLINE addThroughput #-}
addThroughput :: Float -> b -> b
addThroughput storage toProfile = unsafePerformIO $ do
    val <- evaluate toProfile

    gotStackCount <- readIORef stackCount
    gotTimes <- readIORef times
    let time = fromJust $ gotTimes !? gotStackCount
    modifyIORef' times (// [(gotStackCount, first (
        ++ " " ++ show storage ++ "GB"
        ++ " " ++ show (storage / snd time) ++ "gb/s"
        ) time)])

    return val

-- #else
-- profileGeneric :: String -> (b -> IO a) -> b -> b
-- profileGeneric _ _ v = v
-- #endif


profileIO :: String -> IO a -> IO a
profileIO name = profileGeneric name id

profileFoldable :: Foldable t => String -> t b -> t b
profileFoldable name = profileGeneric name (mapM_ evaluate)

profile :: String -> a -> a
profile name = profileGeneric name evaluate

getDivisor :: IO Float
getDivisor = do
    startOs <- getOsTime
    startRdtsc <- getRdtscTime 1
    _getDivisor startOs startRdtsc
    where
        _getDivisor startOs startRdtsc = do
            time <- getOsTime
            rdtscTime <- getRdtscTime 1
            let elapsed = time - startOs

            if elapsed < 100_000 then _getDivisor startOs startRdtsc
            else return $ fromIntegral $ let elapsedRdtsc = rdtscTime - startRdtsc
                          in 1_000_000 * elapsedRdtsc `div` elapsed


startProf :: IO ()
startProf = do
    time <- getRdtscTime 1
    calcedDivisor <- getDivisor
    writeIORef divisor calcedDivisor

    writeIORef startTime (fromIntegral time / calcedDivisor)

addToMap :: Num a => a -> Maybe a -> Maybe a
addToMap x y = Just $ x + fromMaybe 0 y

printTimes :: IO ()
printTimes = do
    curTime <- getRdtscTime 1
    allTimes <- readIORef times
    timeStarted <- readIORef startTime
    arrSize <- readIORef size
    gotDivisor <- readIORef divisor
    let summed = (fromIntegral curTime / gotDivisor) - timeStarted
        summed :: Float
        mapped = foldr (\ (n, v) b -> alter (addToMap v) n b) empty $ take arrSize $ A.elems allTimes
        divved = map (\ (name, time) -> name ++ " " ++ show time ++ "s (" ++ show (time / summed * 100) ++ "%)") $ M.toList mapped
    putStrLn $ foldl (\ a b -> a ++ "\n" ++ b) "" divved
