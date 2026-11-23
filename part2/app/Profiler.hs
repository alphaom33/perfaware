{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE CPP #-}

module Profiler where
import GHC.IO (unsafePerformIO, evaluate)
import Time (getRdtscTime)
import Data.IORef (newIORef, modifyIORef', readIORef, IORef, writeIORef)
import Control.Arrow (Arrow(second))
import Data.Array.Base ((!?), thawIOArray)
import Data.Array (Array, array)
import qualified Data.Array.Base as A
import Data.Array.ST (writeArray)
import Data.Maybe (fromJust, fromMaybe)
import Data.Array.IO (freeze)
import Data.Map (empty, alter)
import qualified Data.Map as M
import Control.Monad (when)
import Debug.Trace (trace)

type Anchors = Array Int (String, Int)

{-# NOINLINE (//) #-}
(//) :: Anchors -> [(Int, (String, Int))] -> Anchors
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
startTime :: IORef Int
startTime = unsafePerformIO $ newIORef 0

addTime :: Int -> String -> Int -> IO ()
addTime i name time = do
    modifyIORef' size (+1)
    modifyIORef' times (// [(i,(name,time))])

getTimesIndex :: Int -> IO (String, Int)
getTimesIndex i = do
    gotTimes <- readIORef times
    return $ fromJust $ gotTimes !? i

modifyTimesIndex :: Int -> (Int -> Int) -> IO ()
modifyTimesIndex i f = do
    gotTimes <- readIORef times
    modifyIORef' times (// [(i, second f $ fromJust $ gotTimes !? i)])

killStack :: Int -> Int -> Int -> IO ()
killStack i ci diff = do
    curr <- getTimesIndex (i - ci)
    if snd curr < 1 then do
        modifyTimesIndex (i - ci) (- diff)
        when (ci < -1) $ killStack i (ci + 1) diff
    else killStack (i - 1) ci diff

#ifdef PROFILE
{-# NOINLINE profileIO #-}
profileGeneric :: String -> (b -> IO a) -> b -> b
profileGeneric name op val = unsafePerformIO $ do
    count <- readIORef stackCount 
    i <- readIORef size
    addTime i name 0

    beginTime <- getRdtscTime 1

    writeIORef stackCount (count + 1)
    _ <- op val
    writeIORef stackCount count

    endTime <- getRdtscTime 1

    let diff = fromIntegral $ endTime - beginTime
    modifyTimesIndex i (+ diff)
    time <- getTimesIndex i 
    when (count > 0) $ killStack (i - count) -count (snd time)
    return val
#else
profileGeneric :: String -> (b -> IO a) -> b -> b
profileGeneric _ _ v = v
#endif

profileIO :: String -> IO a -> IO a
profileIO name = profileGeneric name id

profileFoldable :: Foldable t => String -> t b -> t b
profileFoldable name = profileGeneric name (mapM_ evaluate)

profile :: String -> a -> a
profile name = profileGeneric name evaluate

startProf :: IO ()
startProf = do
    time <- getRdtscTime 1    
    writeIORef startTime $ fromIntegral time

addToMap :: Num a => a -> Maybe a -> Maybe a
addToMap x y = Just $ x + fromMaybe 0 y

printTimes :: IO ()
printTimes = do
    curTime <- getRdtscTime 1
    allTimes <- readIORef times
    timeStarted <- readIORef startTime
    arrSize <- readIORef size
    let summed = fromIntegral curTime - fromIntegral timeStarted
        summed :: Double
        mapped = foldr (\ (n, v) b -> alter (addToMap v) n b) empty $ take arrSize $ A.elems allTimes
        divved = map (\ (name, time) -> name ++ " " ++ show time ++ " (" ++ show (fromIntegral time / summed * 100) ++ "%)") $ M.toList mapped
    putStrLn $ foldl (\ a b -> a ++ "\n" ++ b) "" divved
