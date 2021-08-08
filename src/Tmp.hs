{-# LANGUAGE OverloadedStrings #-}

module Tmp where

import qualified Data.ByteString as BS
import           Codec.Compression.Lzo.Block

type Bytes = BS.ByteString

main :: IO ()
main = do
    let ex1c1  = compress1 ex1
        ex1c9  = compress9 ex1
        ex1c1d = decompress ex1c1 (BS.length ex1)
        ex1c9d = decompress ex1c9 (BS.length ex1)
    print ex1c1
    print ex1c9
    print (BS.length ex1c1)
    print (BS.length ex1c9)
    print ex1c1d
    print ex1c9d

ex1 :: Bytes
ex1 = "gjgggggg jjpoer023fdf,vjdflk gjeo fjfjf f fffkfkkykyiy i m mf s,,ds,sf;s;s;;;"
