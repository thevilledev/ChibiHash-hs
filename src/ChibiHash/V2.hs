{-| V2 implementation of ChibiHash -}
module ChibiHash.V2
    ( chibihash64
    ) where

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (unfoldr)

-- | Prime-like constant used for mixing
k :: Word64
k = 0x2B7E151628AED2A7  -- digits of e

-- | Convert bytes to Word64 using little-endian ordering
load64le :: [Word8] -> Word64
load64le bytes = foldr (\(pos, b) acc -> acc .|. (fromIntegral b `shiftL` (pos * 8)))
                      0
                      (zip [0..7] (take 8 bytes))

-- | Convert bytes to Word32 using little-endian ordering
load32le :: [Word8] -> Word64
load32le bytes = fromIntegral $ foldr (\(pos, b) acc -> acc .|. (fromIntegral b `shiftL` (pos * 8)))
                      (0 :: Word32)
                      (zip [0..3] (take 4 bytes))

-- | Main hash function for V2
chibihash64 :: ByteString -> Word64 -> Word64
chibihash64 input seed = x'
  where
    bytes = BS.unpack input
    len = fromIntegral $ BS.length input
    
    seed2 = ((seed - k) `rotateL` 15) + ((seed - k) `rotateL` 47)
    
    h0 = [ seed
         , seed + k
         , seed2
         , seed2 + k * k `xor` k
         ]
    
    -- Process full blocks and get remaining state
    (h1, remaining) = processBlocks bytes h0
    
    -- Process remaining bytes
    [ha, hb, hc, hd] = processRemaining remaining (length remaining) h1
    
    -- Final mixing steps following the Rust implementation exactly
    h1' = ha + ((hc * k) `rotateL` 31 `xor` (hc `shiftR` 31))
    h2' = hb + ((hd * k) `rotateL` 31 `xor` (hd `shiftR` 31))
    h3' = h1' * k
    h4' = h3' `xor` (h3' `shiftR` 31)
    h5' = h2' + h4'
    
    x = len * k
    x' = ((x `xor` (x `rotateL` 29)) + seed) `xor` h5'
    x'' = x' `xor` (x' `rotateL` 15) `xor` (x' `rotateL` 42)
    x''' = x'' * k
    final = x''' `xor` (x''' `rotateL` 13) `xor` (x''' `rotateL` 31)
    
process8ByteChunks :: [Word8] -> [Word64] -> [Word64]
process8ByteChunks bytes [ha, hb, hc, hd]
    | length bytes >= 8 =
        let ha' = ha `xor` load32le bytes
            ha'' = ha' * k
            hb' = hb `xor` load32le (drop 4 bytes)
            hb'' = hb' * k
        in process8ByteChunks (drop 8 bytes) [ha'', hb'', hc, hd]
    | otherwise = [ha, hb, hc, hd]
process8ByteChunks _ _ = error "Invalid state"

processStripe :: ([Word64], Int, [Word8]) -> [Word64]
processStripe (state@[h0, h1, h2, h3], i, stripe) | i >= 0 && i < 4 =
    let v = load64le stripe
        hi' = (v + (state !! i)) * k
        nextIdx = (i + 1) .&. 3
        next = (state !! nextIdx) + (v `rotateL` 27)
    in case i of
        0 -> [hi', next, h2, h3]
        1 -> [h0, hi', next, h3]
        2 -> [h0, h1, hi', next]
        3 -> [next, h1, h2, hi']
        _ -> error "Invalid index"
processStripe _ = error "Invalid state: expected 4 hash values"

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
-- | Process remaining bytes
processRemaining :: [Word8] -> Int -> [Word64] -> [Word64]
processRemaining bytes len [ha, hb, hc, hd]
    | len >= 8 = process8ByteChunks bytes [ha, hb, hc, hd]
    | len >= 4 = 
        let hc' = hc `xor` load32le bytes
            hd' = hd `xor` load32le (drop (len - 4) bytes)
        in [ha, hb, hc', hd']
    | len > 0 = 
        let hc' = hc `xor` fromIntegral (head bytes)
            hd' = hd `xor` (fromIntegral (bytes !! (len `div` 2)) .|. 
                           (fromIntegral (last bytes) `shiftL` 8))
        in [ha, hb, hc', hd']
    | otherwise = [ha, hb, hc, hd]
processRemaining _ _ _ = error "Invalid state"

-- | Split a list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n) 