# ChibiHash-hs

[<img alt="Hackage Version" src="https://img.shields.io/hackage/v/ChibiHash">](https://hackage.haskell.org/package/ChibiHash)

Haskell port of [N-R-K/ChibiHash](https://github.com/N-R-K/ChibiHash). See the article [ChibiHash: A small, fast 64-bit hash function](https://nrk.neocities.org/articles/chibihash) for more information.

All credit for the algorithm goes to [N-R-K](https://github.com/N-R-K).

## Usage 

```haskell
module Main (main) where

import ChibiHash (chibihash64)
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let text = "Hello, ChibiHash!"
    putStrLn $ "Input text: " ++ show text
    putStrLn $ "Hash (seed 0): " ++ show (chibihash64 (C8.pack text) 0)
```

You may also run the example program with `cabal run`.

## Tests

Run tests with `cabal test`. Both v1 and v2 are tested.

## License

MIT.
