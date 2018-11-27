module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (mapAccumR)

type Lat = Int

parseLat :: ByteString -> Lat
parseLat = parse

parse :: ByteString -> Int
parse bs = sum [f * x | (f, x) <- zip fs lats]
    where
        lats = map fromIntegral $ BL.unpack $ BL.take 3 $ BL.drop 76 bs :: [Int]
        fs = take (length lats) (iterate (256*) 1)

main :: IO ()
main = do
    f <- BL.readFile "001"
    BLC.putStrLn $ BL.take 9 $ BL.drop 12 f
    print $ parseLat f
