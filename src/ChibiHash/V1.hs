{-| V1 implementation of ChibiHash -}
module ChibiHash.V1
    ( chibihash64
    ) where

import Data.Word
import Data.Bits
import Data.List (unfoldr)
import Data.Int (Int64)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- | Prime-like constants used for mixing
p1, p2, p3 :: Word64
p1 = 0x2B7E151628AED2A5  -- Used in main block processing
p2 = 0x9E3793492EEDC3F7  -- Used in remaining bytes processing
p3 = 0x3243F6A8885A308D  -- Used in 2-byte chunk processing

-- | Convert 8 bytes into a Word64 using little-endian ordering
-- Each byte is shifted left by its position (0, 8, 16, ...) and combined
load64le :: [Word8] -> Word64
load64le bytes = foldr (\(pos, b) acc -> acc .|. (fromIntegral b `shiftL` pos))
                      0
                      (zip [0,8..] (take 8 bytes))

-- | Main hash function that processes input in several stages:
-- 1. Process full 32-byte blocks
-- 2. Process remaining bytes (< 32 bytes)
-- 3. Apply final mixing function
chibihash64 :: ByteString -> Word64 -> Word64
chibihash64 input seed = finalMix x
  where
    bytes = BS.unpack input
    len = fromIntegral $ BS.length input

    -- Initial state
    h0 = [p1, p2, p3, seed]

    -- Process full 32-byte blocks
    (h1, remaining) = processBlocks bytes h0

    -- Process remaining bytes
    h2 = processRemaining remaining len h1

    -- Final mixing
    (ha', hb', hc', hd') = case h2 of
        [a, b, c, d] -> (a, b, c, d)
        _ -> error "Impossible: hash state must contain exactly 4 elements"

    x = seed  -- Start with seed
        `xor` (ha' * ((hc' `shiftR` 32) .|. 1))
        `xor` (hb' * ((hd' `shiftR` 32) .|. 1))
        `xor` (hc' * ((ha' `shiftR` 32) .|. 1))
        `xor` (hd' * ((hb' `shiftR` 32) .|. 1))

-- | Process input in 32-byte blocks (4 lanes of 8 bytes each)
-- Returns the updated hash state and any remaining bytes
processBlocks :: [Word8] -> [Word64] -> ([Word64], [Word8])
processBlocks input h
    | length input < 32 = (h, input)  -- Not enough bytes for a full block
    | otherwise = 
        let (block, rest) = splitAt 32 input
            h' = processBlock block h
        in processBlocks rest h'
    where
        -- Process each 8-byte lane within the 32-byte block
        processBlock block hashState =
            foldl processLane hashState (zip [0..3] (chunksOf 8 block))
        -- Process a single 8-byte lane:
        -- 1. Load 8 bytes as Word64
        -- 2. XOR with current state and multiply
        -- 3. Update next state with rotated value
        processLane hashState (i, lane) =
            let v = load64le lane
                hi = hashState !! i
                hi' = (hi `xor` v) * p1  -- Mix current lane
                nextIdx = (i + 1) .&. 3   -- Circular index for next lane
                next = (v `shiftL` 40) .|. (v `shiftR` 24)  -- Rotate input by 40 bits
                h' = take i hashState ++ [hi'] ++ drop (i + 1) hashState    -- Update current lane
                h'' = take nextIdx h' ++ [h' !! nextIdx `xor` next] ++ drop (nextIdx + 1) h'  -- Update next lane
            in h''

-- | Process remaining bytes that didn't fill a complete 32-byte block
-- Handles:
-- 1. Length mixing into first hash value
-- 2. Single odd byte (if present)
-- 3. Remaining 8-byte chunks
-- 4. Final 2-byte chunks
processRemaining :: [Word8] -> Int64 -> [Word64] -> [Word64]
processRemaining bytes len _state@[a, b, c, d] =
    let -- First add length mix to h[0]
        ha' = a + ((fromIntegral len `shiftL` 32) .|. (fromIntegral len `shiftR` 32))

        -- Handle single byte if length is odd
        (ha'', bytes', len') = if not (null bytes) && (length bytes .&. 1) == 1
            then (ha' `xor` fromIntegral (head bytes), tail bytes, length bytes - 1)
            else (ha', bytes, length bytes)

        -- Multiply and shift h[0]
        ha''' = ha'' * p2
        ha4 = ha''' `xor` (ha''' `shiftR` 31)

        -- Process 8-byte chunks into h[1], h[2], h[3]
        h1 = process8ByteChunks bytes' 1 [ha4, b, c, d]

        -- Process remaining 2-byte chunks
        h2 = process2ByteChunks (drop (len' .&. complement 7) bytes') 0 h1
    in h2
processRemaining _ _ _ = error "Unexpected state: processRemaining requires exactly 4 elements in the state"

-- | Process 8-byte chunks into h[1], h[2], h[3]
process8ByteChunks :: [Word8] -> Int -> [Word64] -> [Word64]
process8ByteChunks bs i h
    | length bs >= 8 && i < 4 =
        let v = load64le bs
            hi = h !! i
            hi' = hi `xor` v
            hi'' = hi' * p2
            hi''' = hi'' `xor` (hi'' `shiftR` 31)
            h' = take i h ++ [hi'''] ++ drop (i + 1) h
        in process8ByteChunks (drop 8 bs) (i + 1) h'
    | otherwise = h

-- | Process remaining 2-byte chunks
process2ByteChunks :: [Word8] -> Int -> [Word64] -> [Word64]
process2ByteChunks bs i h
    | length bs >= 2 =
        let v = fromIntegral (head bs) .|. (fromIntegral (bs !! 1) `shiftL` 8)
            hi = h !! i
            hi' = hi `xor` v
            hi'' = hi' * p3
            hi''' = hi'' `xor` (hi'' `shiftR` 31)
            h' = take i h ++ [hi'''] ++ drop (i + 1) h
        in process2ByteChunks (drop 2 bs) ((i + 1) .&. 3) h'
    | otherwise = h

-- | Final mixing function to improve avalanche effect
-- Applies a series of xor, shift, and multiply operations
finalMix :: Word64 -> Word64
finalMix x = x3
  where
    -- Each step: XOR with right shift, then multiply by a large prime
    x1 = (x `xor` (x `shiftR` 27)) * 0x3C79AC492BA7B653
    x2 = (x1 `xor` (x1 `shiftR` 33)) * 0x1C69B3F74AC4AE35
    x3 = x2 `xor` (x2 `shiftR` 27)

-- | Split a list into chunks of size n
-- Used to break input into 8-byte lanes
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)