{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List                  (mapAccumR)
import           GHC.Int                    (Int64)
import           System.Environment         (getArgs)

startDateIndex :: Int64
startDateIndex = 9

endDateIndex :: Int64
endDateIndex = 12

latIndex :: Int64
latIndex = 76

lonIndex :: Int64
lonIndex = 80

parseDateTime :: ByteString -> ByteString
parseDateTime = BL.take startDateIndex . BL.drop endDateIndex


parse :: Int64 -> ByteString -> Double
parse index bs = realToFrac res / (100000 :: Double)
    where
        lats = map fromIntegral $ BL.unpack $ BL.take 3 $ BL.drop index bs :: [Int]
        fs = take (length lats) (iterate (256*) 1)
        res = sum [f * x | (f, x) <- zip fs lats]

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    f <- BL.readFile filePath
    let dateTime = parseDateTime f
    let lat = BLC.pack $ show $ parse latIndex f
    let lon = BLC.pack $ show $ parse lonIndex f
    BLC.putStrLn $ BL.intercalate "," [dateTime, lat, lon]
