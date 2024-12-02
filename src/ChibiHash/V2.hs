{-| V2 implementation of ChibiHash 

    This is a 64-bit non-cryptographic hash function optimized for:
    - Fast performance on short strings
    - Good distribution of hash values
    - Simple implementation with no lookup tables

    Version 2 improvements over V1, from the original C implementation:

    - Faster performance on short strings (42 cycles/hash vs 34 cycles/hash)
    - Improved seeding that affects all 256 bits of internal state
    - Better mixing in bulk data processing
    - Passes all 252 tests in smhasher3 (commit 34093a3), v1 failed 3.

-}
module ChibiHash.V2
    ( chibihash64
    ) where

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Prime-like constant used for mixing, derived from digits of e
k :: Word64
k = 0x2B7E151628AED2A7

-- | Convert bytes to Word64 using little-endian ordering
-- Takes 8 bytes and combines them into a single 64-bit word
load64le :: [Word8] -> Word64
load64le bytes = 
    let lo = load32le bytes
        hi = load32le (drop 4 bytes)
    in lo .|. (hi `shiftL` 32)

-- | Convert bytes to Word32 using little-endian ordering
-- Takes 4 bytes and combines them into the lower 32 bits of a Word64
load32le :: [Word8] -> Word64
load32le bytes = 
    let b0 = fromIntegral (head bytes)
        b1 = fromIntegral (bytes !! 1) `shiftL` 8
        b2 = fromIntegral (bytes !! 2) `shiftL` 16
        b3 = fromIntegral (bytes !! 3) `shiftL` 24
    in b0 .|. b1 .|. b2 .|. b3

-- | Basic arithmetic operations used throughout the hash function
add, subtract, mul :: Word64 -> Word64 -> Word64
add a b = a + b
subtract a b = a - b
mul a b = a * b

-- | Main hash function for V2
-- Takes a ByteString input and 64-bit seed value
-- Returns a 64-bit hash value
chibihash64 :: ByteString -> Word64 -> Word64
chibihash64 input seed = 
    let bytes = BS.unpack input
        len = fromIntegral (BS.length input) :: Word64
        
        -- Initialize state with seed-dependent values
        seed2 = ((seed `ChibiHash.V2.subtract` k) `rotateL` 15) `add` 
                ((seed `ChibiHash.V2.subtract` k) `rotateL` 47)
        
        h0 = [ seed
             , seed `add` k
             , seed2
             , seed2 `add` ((k `mul` k) `xor` k)
             ]
        
        -- Process input in stages
        (h1, remaining) = processBlocks bytes h0  -- Process 32-byte blocks
        h2 = processRemaining remaining (length remaining) h1  -- Handle remaining bytes
        
    in case h2 of
        [ha, hb, hc, hd] -> 
            let -- Final mixing steps
                h_final_0 = ha `add` ((hc `mul` k) `rotateL` 31 `xor` (hc `shiftR` 31))
                h_final_1 = hb `add` ((hd `mul` k) `rotateL` 31 `xor` (hd `shiftR` 31))
                h_final_0' = h_final_0 `mul` k
                h_final_0'' = h_final_0' `xor` (h_final_0' `shiftR` 31)
                h_final_1' = h_final_1 `add` h_final_0''
                
                -- Length-dependent mixing
                x = len `mul` k
                x' = x `xor` (x `rotateL` 29)
                x'' = x' `add` seed
                x''' = x'' `xor` h_final_1'
                x'''' = x''' `xor` (x''' `rotateL` 15) `xor` (x''' `rotateL` 42)
                x''''' = x'''' `mul` k
                final = x''''' `xor` (x''''' `rotateL` 13) `xor` (x''''' `rotateL` 31)
            in final
        _ -> error "Invalid state: processRemaining must return 4 values"

-- | Process input in 32-byte blocks
-- Each block is split into four 8-byte stripes
processBlocks :: [Word8] -> [Word64] -> ([Word64], [Word8])
processBlocks input h
    | length input < 32 = (h, input)
    | otherwise =
        let (block, rest) = splitAt 32 input
            stripes = chunksOf 8 block
            h' = foldl (\acc (i, s) -> processStripe (acc, i, s)) 
                      h 
                      (zip [0..3] stripes)
        in processBlocks rest h'

-- | Process an 8-byte stripe within a block
-- Updates the 4-element state array based on stripe index
processStripe :: ([Word64], Int, [Word8]) -> [Word64]
processStripe (state, i, stripe) | i >= 0 && i < 4 =
    let v = load64le stripe
        hi' = (v `add` (state !! i)) `mul` k
        nextIdx = (i + 1) .&. 3
        next = (state !! nextIdx) `add` (v `rotateL` 27)
    in case i of
        0 -> [hi', next, state !! 2, state !! 3]
        1 -> [head state, hi', next, state !! 3]
        2 -> [head state, state !! 1, hi', next]
        3 -> [next, state !! 1, state !! 2, hi']
        _ -> error "Invalid index"
processStripe _ = error "Invalid state: expected 4 hash values"

-- | Process remaining bytes after block processing
-- Handles different cases based on number of remaining bytes:
-- - 8 or more bytes: process in 8-byte chunks
-- - 4-7 bytes: special handling with two 32-bit reads
-- - 1-3 bytes: special handling for very short remainders
processRemaining :: [Word8] -> Int -> [Word64] -> [Word64]
processRemaining bytes len state@[ha, hb, hc, hd]
    | len >= 8 = 
        let (chunk, rest) = splitAt 8 bytes
            ha' = ha `xor` load32le chunk
            ha'' = ha' `mul` k
            hb' = hb `xor` load32le (drop 4 chunk)
            hb'' = hb' `mul` k
            remaining_len = len - 8
        in processRemaining rest remaining_len [ha'', hb'', hc, hd]
    | len >= 4 = 
        let hc' = hc `xor` load32le bytes
            hd' = hd `xor` load32le (drop (len - 4) bytes)
        in [ha, hb, hc', hd']
    | len > 0 = 
        let hc' = hc `xor` fromIntegral (head bytes)
            mid_byte = fromIntegral (bytes !! (len `div` 2))
            last_byte = fromIntegral (last bytes) `shiftL` 8
            hd' = hd `xor` (mid_byte .|. last_byte)
        in [ha, hb, hc', hd']
    | otherwise = state
processRemaining _ _ _ = error "Invalid state"

-- | Split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)
  where
    unfoldr f x = case f x of
                    Just (y, ys) -> y : unfoldr f ys
                    Nothing -> []