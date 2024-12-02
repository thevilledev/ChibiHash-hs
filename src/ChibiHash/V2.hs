{-| V2 implementation of ChibiHash -}
module ChibiHash.V2
    ( chibihash64
    ) where

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Debug.Trace (trace)

-- | Prime-like constant used for mixing
k :: Word64
k = 0x2B7E151628AED2A7  -- digits of e

-- | Convert bytes to Word64 using little-endian ordering
load64le :: [Word8] -> Word64
load64le bytes = 
    let lo = load32le bytes
        hi = load32le (drop 4 bytes)
    in lo .|. (hi `shiftL` 32)

-- | Convert bytes to Word32 using little-endian ordering
load32le :: [Word8] -> Word64
load32le bytes = 
    let b0 = fromIntegral (head bytes)
        b1 = fromIntegral (bytes !! 1) `shiftL` 8
        b2 = fromIntegral (bytes !! 2) `shiftL` 16
        b3 = fromIntegral (bytes !! 3) `shiftL` 24
    in b0 .|. b1 .|. b2 .|. b3

add, subtract, mul :: Word64 -> Word64 -> Word64
add a b = a + b
subtract a b = a - b
mul a b = a * b

-- Helper for debug logging
debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ ": " ++ show x) x

-- | Main hash function for V2
chibihash64 :: ByteString -> Word64 -> Word64
chibihash64 input seed = 
    let bytes = BS.unpack input
        len = debug "len" $ fromIntegral (BS.length input) :: Word64
        
        seed2 = debug "seed2" $ ((seed `ChibiHash.V2.subtract` k) `rotateL` 15) `add` 
                               ((seed `ChibiHash.V2.subtract` k) `rotateL` 47)
        
        h0 = debug "initial state" [ seed
                                  , seed `add` k
                                  , seed2
                                  , seed2 `add` ((k `mul` k) `xor` k)
                                  ]
        
        (h1, remaining) = debug "after blocks" $ processBlocks bytes h0
        h2 = debug "after remaining" $ processRemaining remaining (length remaining) h1
    in case h2 of
        [ha, hb, hc, hd] -> 
            let h_final_0 = debug "h_final_0" $ ha `add` ((hc `mul` k) `rotateL` 31 `xor` (hc `shiftR` 31))
                h_final_1 = debug "h_final_1" $ hb `add` ((hd `mul` k) `rotateL` 31 `xor` (hd `shiftR` 31))
                h_final_0' = debug "h_final_0'" $ h_final_0 `mul` k
                h_final_0'' = debug "h_final_0''" $ h_final_0' `xor` (h_final_0' `shiftR` 31)
                h_final_1' = debug "h_final_1'" $ h_final_1 `add` h_final_0''
                
                x = debug "x" $ len `mul` k
                x' = debug "x'" $ x `xor` (x `rotateL` 29)
                x'' = debug "x''" $ x' `add` seed
                x''' = debug "x'''" $ x'' `xor` h_final_1'
                x'''' = debug "x''''" $ x''' `xor` (x''' `rotateL` 15) `xor` (x''' `rotateL` 42)
                x''''' = debug "x'''''" $ x'''' `mul` k
                final = debug "final" $ x''''' `xor` (x''''' `rotateL` 13) `xor` (x''''' `rotateL` 31)
            in final
        _ -> error "Invalid state: processRemaining must return 4 values"

processBlocks :: [Word8] -> [Word64] -> ([Word64], [Word8])
processBlocks input h
    | length input < 32 = (h, input)
    | otherwise =
        let (block, rest) = splitAt 32 input
            stripes = chunksOf 8 block
            h' = debug "process block" $ foldl (\acc (i, s) -> processStripe (acc, i, s)) 
                                             h 
                                             (zip [0..3] stripes)
        in processBlocks rest h'

processStripe :: ([Word64], Int, [Word8]) -> [Word64]
processStripe (state, i, stripe) | i >= 0 && i < 4 =
    let v = debug ("stripe " ++ show i) $ load64le stripe
        hi' = debug ("hi' " ++ show i) $ (v `add` (state !! i)) `mul` k
        nextIdx = (i + 1) .&. 3
        next = debug ("next " ++ show i) $ (state !! nextIdx) `add` (v `rotateL` 27)
    in case i of
        0 -> debug "stripe result 0" [hi', next, state !! 2, state !! 3]
        1 -> debug "stripe result 1" [head state, hi', next, state !! 3]
        2 -> debug "stripe result 2" [head state, state !! 1, hi', next]
        3 -> debug "stripe result 3" [next, state !! 1, state !! 2, hi']
        _ -> error "Invalid index"
processStripe _ = error "Invalid state: expected 4 hash values"

processRemaining :: [Word8] -> Int -> [Word64] -> [Word64]
processRemaining bytes len state@[ha, hb, hc, hd]
    | len >= 8 = 
        let (chunk, rest) = splitAt 8 bytes
            ha' = debug "ha xor" $ ha `xor` load32le chunk
            ha'' = debug "ha mul" $ ha' `mul` k
            hb' = debug "hb xor" $ hb `xor` load32le (drop 4 chunk)
            hb'' = debug "hb mul" $ hb' `mul` k
            remaining_len = len - 8
        in processRemaining rest remaining_len [ha'', hb'', hc, hd]
    | len >= 4 = 
        let hc' = debug "hc xor" $ hc `xor` load32le bytes
            hd' = debug "hd xor" $ hd `xor` load32le (drop (len - 4) bytes)
        in [ha, hb, hc', hd']
    | len > 0 = 
        let hc' = debug "hc xor small" $ hc `xor` fromIntegral (head bytes)
            mid_byte = fromIntegral (bytes !! (len `div` 2))
            last_byte = fromIntegral (last bytes) `shiftL` 8
            hd' = debug "hd xor small" $ hd `xor` (mid_byte .|. last_byte)
        in [ha, hb, hc', hd']
    | otherwise = state
processRemaining _ _ _ = error "Invalid state"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)
  where
    unfoldr f x = case f x of
                    Just (y, ys) -> y : unfoldr f ys
                    Nothing -> []