{-|
Module      : ChibiHash
Description : A simple and fast 64-bit hash function
Copyright   : (c) Ville Vesilehto, 2024
License     : MIT
Maintainer  : ville@vesilehto.fi
Stability   : experimental
Portability : portable

ChibiHash is a simple and fast 64-bit hash function suitable for hash tables and
hash-based data structures. This module provides both V1 and V2 implementations.

Example usage:

@
import ChibiHash (chibihash64)  -- Uses V1 by default
import qualified ChibiHash.V2 as V2

main :: IO ()
main = do
    let input = BS.pack [1,2,3,4]
    let seed = 0
    print $ chibihash64 input seed        -- V1 hash
    print $ V2.chibihash64 input seed     -- V2 hash
@
-}

module ChibiHash
  ( -- * Hash Functions
    chibihash64           -- Default (V1 implementation)
  , chibihash64V1         -- Explicit V1 implementation
  , chibihash64V2         -- Explicit V2 implementation
  ) where

import qualified ChibiHash.V1 as V1
import qualified ChibiHash.V2 as V2

import Data.ByteString (ByteString)
import Data.Word (Word64)

chibihash64 :: ByteString -> Word64 -> Word64
chibihash64 = V1.chibihash64

chibihash64V1 :: ByteString -> Word64 -> Word64
chibihash64V1 = V1.chibihash64

chibihash64V2 :: ByteString -> Word64 -> Word64
chibihash64V2 = V2.chibihash64