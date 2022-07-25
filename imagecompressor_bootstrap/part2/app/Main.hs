module Main where

import Codec.Picture
import Data.ByteString




main :: IO ()
main = do
    l <- Data.ByteString.readFile "image.jpg"
    let x = decodeImage l
 
    return ()

