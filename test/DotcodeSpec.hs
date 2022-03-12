module DotcodeSpec (spec) where

import qualified Codec.Dotcode as Dotcode
import GHC.Natural (Natural)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck.Instances.Natural ()

testEncodeDecode :: Natural -> Expectation
testEncodeDecode i = Dotcode.decode (fst $ Dotcode.encode i) `shouldBe` Just i

spec :: Spec
spec = do
  describe "encode" $ do
    it "encodes" $ do
      Dotcode.encode 0b0100001101000110110110011011100101000011110010
        `shouldBe` (0b1101000101011001000110110110011011100101000011110001000110010001, 8)
      Dotcode.encode 0b0100001011000110110010111011010101000100000110
        `shouldBe` (0b1101000101010101000110110010111011010101000100000011010110111011, 8)

    it "encodes m" $ do
      Dotcode.encode 374901746482285619
        `shouldBe` (0b110000101011001100011111010110010011011110000110000000000110000101001100100110101, 9)

  describe "encode and decode" $ do
    it "should give the same result" $
      do
        mapM_
          testEncodeDecode
          [ 0b1,
            0b10,
            0b101,
            0b1111,
            0b1111111,
            0b11111111,
            0b111111111
          ]
    modifyMaxSuccess (const 1000) $
      prop "should give the same result with QuickCheck" $
        \x -> testEncodeDecode x
