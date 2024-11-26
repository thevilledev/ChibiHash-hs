# ChibiHash-hs

(work in progress)

Haskell port of [N-R-K/ChibiHash](https://github.com/N-R-K/ChibiHash). See the article [ChibiHash: A small, fast 64-bit hash function](https://nrk.neocities.org/articles/chibihash) for more information.

All credit for the algorithm goes to [N-R-K](https://github.com/N-R-K).

## Usage 

```haskell
module Main where

import ChibiHash (chibihash64)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    let input = BS.pack [1,2,3,4]
    let seed = 0
    print $ chibihash64 input seed
```

## Tests

Run tests with `cabal test`.

## License

MIT.
