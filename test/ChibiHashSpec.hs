module ChibiHashSpec (spec) where

import Test.Hspec
import ChibiHash (chibihash64)
import Data.ByteString.Char8 (pack)

spec :: Spec
spec = describe "ChibiHash" $ do
    describe "chibihash64" $ do
        it "handles empty string" $ do
            chibihash64 (pack "") 0 `shouldBe` 0x9EA80F3B18E26CFB
            chibihash64 (pack "") 55555 `shouldBe` 0x2EED9399FC4AC7E5

        it "handles short strings" $ do
            chibihash64 (pack "hi") 0 `shouldBe` 0xAF98F3924F5C80D6
            chibihash64 (pack "123") 0 `shouldBe` 0x893A5CCA05B0A883
            chibihash64 (pack "abcdefgh") 0 `shouldBe` 0x8F922660063E3E75
            chibihash64 (pack "Hello, world!") 0 `shouldBe` 0x5AF920D8C0EBFE9F

        it "handles exactly 32-byte string" $ do
            chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456") 0
                `shouldBe` 0x2EF296DB634F6551

        it "handles string longer than 32 bytes" $ do
            chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456789") 0
                `shouldBe` 0x0F56CF3735FFA943
