module Main (main) where

import ChibiHash (chibihash64) -- v1 by default
import qualified ChibiHash.V2 as V2 -- v2 explicitly
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    -- Example 1: Hash a simple string
    let text = "Hello, ChibiHash!"
    putStrLn $ "Input text: " ++ show text
    putStrLn $ "Hash (seed 0): " ++ show (chibihash64 (C8.pack text) 0)
    putStrLn $ "Hash (seed 42): " ++ show (chibihash64 (C8.pack text) 42)
    putStrLn ""

    -- Example 2: Hash some binary data
    let binary = BS.pack [1, 2, 3, 4, 5]
    putStrLn $ "Input bytes: " ++ show binary
    putStrLn $ "Hash (seed 0): " ++ show (chibihash64 binary 0)

    -- Example 3: Hash an empty string
    putStrLn $ "\nHash of empty string: " ++ show (chibihash64 BS.empty 0)

    -- Example 4: Hash a string with ChibiHash v2
    let text2 = "Hello, ChibiHash!"
    putStrLn $ "\nInput text: " ++ show text2
    putStrLn $ "Hash (seed 0): " ++ show (V2.chibihash64 (C8.pack text2) 0)
