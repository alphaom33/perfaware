{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where
import System.Environment (getArgs)
import System.Random
import Time
import Data.Word (Word64)
import Profiler (printTimes, profileIO, startProf, profile)
import Control.Exception (evaluate)

type Point = (Double, Double)

main :: IO ()
main = do
    startProf
    divisor <- getDivisor
    print divisor
    _ <- evaluate $ profile "a" $ sum [0..1000000]
    printTimes
    -- args <- getArgs
    -- if isJust $ args !? 3 then do
    --     json <- readFile $ args !! 3
    --     print $ parseJson json
    -- else generate

getDivisor :: IO Word64
getDivisor = do
    startOs <- getOsTime
    startRdtsc <- getRdtscTime 1
    _getDivisor startOs startRdtsc
    where
        _getDivisor startOs startRdtsc = profileIO "asdf" $ do
            time <- getOsTime
            rdtscTime <- profileIO "bsdf" $ getRdtscTime 1
            let elapsed = time - startOs

            if elapsed < 100_000 then _getDivisor startOs startRdtsc
            else return $ let elapsedRdtsc = rdtscTime - startRdtsc
                          in 1_000_000 * elapsedRdtsc `div` elapsed
                
                
generate :: IO ()
generate = do
    args <- getArgs
    let
        gen = mkStdGen $ read $ args !! 1
        numbers = zip (uniformRs (-180.0, 180.0) gen :: [Double]) (uniformRs (-90.0, 90.0) gen :: [Double])
        pointPairs = (if head args == "cluster"
                        then clusterGenerate 16 numbers gen
                        else uniformGenerate numbers)
        finitified = take (read $ args !! 2) pointPairs
    putStrLn $ jsonify finitified
    putStrLn $ "Expected sum: " ++ show (sum (map haversine finitified) / read (args !! 2))

pointify :: [(String, Double)] -> (Point, Point)
pointify [(_, x0),(_,y0),(_,x1),(_,y1)] = ((x0, y0),(x1,y1))
pointify a = error $ show a

uniformGenerate :: [Point] -> [(Point, Point)]
uniformGenerate ((x0,y0):(x1,y1):rest) = ((x0,y0), (x1,y1)) : uniformGenerate rest
uniformGenerate _ = error "what"

clusterGenerate :: Int -> [Point] -> StdGen -> [(Point, Point)]
clusterGenerate numClusters _points gen = 
    let
        generated = _clusterGenerate $ drop numClusters _points
        isolated = [x | (i, x) <- zip [0..] generated, num <- [0..numClusters - 1], i `mod` numClusters == num]
    in pairifify isolated
    where
        arounds = zip (uniformRs (-90.0, 90.0) gen :: [Double]) (take numClusters _points)

        _clusterGenerate points = zipWith (\ (x0,y0) (scale, (centerx, centery)) -> (x0 / 180.0 * scale + centerx, y0 / 180.0 * scale + centery)) points arounds ++ _clusterGenerate (drop 16 points)
        pairifify (point1:point2:rest) = (point1, point2) : pairifify rest
        pairifify _ = error "huh"

jsonify :: [(Point, Point)] -> String
jsonify _p = "{pairs: [\n" ++ _jsonify _p ++ "\n]}"
    where
        jsonate ((x0,y0),(x1,y1)) = "\t{x0: " ++ show x0 ++ ", y0: " ++ show y0 ++ ", x1: " ++ show x1 ++ ", y1: " ++ show y1 ++ "}"
        _jsonify [point] = jsonate point
        _jsonify (point:ps) = jsonate point ++ ",\n" ++ _jsonify ps
        _jsonify [] = error "who at"

rad :: Double -> Double
rad deg = deg * pi / 180.0

earthRadius :: Double
earthRadius = 6372.8

haversine :: (Point, Point) -> Double
haversine ((x0,y0),(x1,y1)) = let
    lon0 = rad x0
    lat0 = rad y0
    lon1 = rad x1
    lat1 = rad y1
    (dLat, dLon) = (lat1 - lat0, lon1 - lon0)
    square = 2 :: Int
    a = sin (dLat / 2.0) ^ square + cos lat0 * cos lat1 * sin (dLon / 2.0) ^ square
    c = 2.0 * asin (sqrt a)
    in earthRadius * c
