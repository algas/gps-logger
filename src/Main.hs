module Main where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Geodetic.LL           (LL (..))
import           Data.List                  (mapAccumR)
import           GHC.Int                    (Int64)


parse :: Int64 -> ByteString -> Double
parse index bs = realToFrac res / (100000 :: Double)
    where
        lats = map fromIntegral $ BL.unpack $ BL.take 3 $ BL.drop index bs :: [Int]
        fs = take (length lats) (iterate (256*) 1)
        res = sum [f * x | (f, x) <- zip fs lats]

main :: IO ()
main = do
    f <- BL.readFile "001"
    BLC.putStrLn $ BL.take 9 $ BL.drop 12 f
    let ll = LL {_lat = parse 76 f, _lon = parse 80 f}
    print ll
