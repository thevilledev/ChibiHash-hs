module ChibiHashSpec (spec) where

import Test.Hspec
import qualified ChibiHash.V1 as V1
import qualified ChibiHash.V2 as V2
import Data.ByteString.Char8 (pack)


spec :: Spec
spec = describe "ChibiHash" $ do
    describe "V1.chibihash64" $ do
        it "handles empty string" $ do
            V1.chibihash64 (pack "") 0 `shouldBe` 0x9EA80F3B18E26CFB
            V1.chibihash64 (pack "") 55555 `shouldBe` 0x2EED9399FC4AC7E5

        it "handles short strings" $ do
            V1.chibihash64 (pack "hi") 0 `shouldBe` 0xAF98F3924F5C80D6
            V1.chibihash64 (pack "123") 0 `shouldBe` 0x893A5CCA05B0A883
            V1.chibihash64 (pack "abcdefgh") 0 `shouldBe` 0x8F922660063E3E75
            V1.chibihash64 (pack "Hello, world!") 0 `shouldBe` 0x5AF920D8C0EBFE9F

        it "handles exactly 32-byte string" $ do
            V1.chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456") 0
                `shouldBe` 0x2EF296DB634F6551

        it "handles string longer than 32 bytes" $ do
            V1.chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456789") 0
                `shouldBe` 0x0F56CF3735FFA943

    describe "V2.chibihash64" $ do
        it "matches Rust implementation test vectors" $ do
            V2.chibihash64 (pack "") 55555 `shouldBe` 0x58AEE94CA9FB5092
            V2.chibihash64 (pack "") 0 `shouldBe` 0xD4F69E3ECCF128FC
            V2.chibihash64 (pack "hi") 0 `shouldBe` 0x92C85CA994367DAC
            V2.chibihash64 (pack "123") 0 `shouldBe` 0x788A224711FF6E25
            V2.chibihash64 (pack "abcdefgh") 0 `shouldBe` 0xA2E39BE0A0689B32
            V2.chibihash64 (pack "Hello, world!") 0 `shouldBe` 0xABF8EB3100B2FEC7
            V2.chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456") 0 
                `shouldBe` 0x90FC5DB7F56967FA
            V2.chibihash64 (pack "qwertyuiopasdfghjklzxcvbnm123456789") 0 
                `shouldBe` 0x6DCDCE02882A4975
