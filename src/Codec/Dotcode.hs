module Codec.Dotcode (encode, decode) where

import Data.Bits
import Data.Function ((&))
import Data.Word (Word8)
import GHC.Num (Natural, naturalLog2)

-- we abuse Natural as a list of bits, the downside is that leading 0's are lost

crcBitLength :: Int
crcBitLength = 8

-- only valid for numbers >= 1
encode :: Natural -> (Natural, Int)
encode n = do
  let cornersBitAmount = 4 + 2 * 3
      crc = calculateCrc n
      amountOfUsedBits = amountOfBitsForInteger n + (cornersBitAmount + fromIntegral crcBitLength)
      matrixWidth = calcMatrixWidth amountOfUsedBits
      amountOfZeroPadding = matrixWidth * matrixWidth - amountOfUsedBits
      -- use a 1 in front so 0's are not lost when stored in a Natural
      -- the 1 will be replaced with a 0 later
      paddingNumber = 0b1 `shiftL` (fromIntegral amountOfZeroPadding - 1)
      numberAndCrc = pushBits n (fromIntegral crc) crcBitLength
      ncLength = amountOfBitsForInteger n + crcBitLength
      (allInfo, allInfoLength) =
        if amountOfZeroPadding > 0
          then (pushBits paddingNumber numberAndCrc ncLength, ncLength + amountOfZeroPadding)
          else (numberAndCrc, ncLength)

  let start = 0b11
  let amountFirstRow = matrixWidth - 3
  let amountMiddleSecondRow = matrixWidth - 4
  -- todo: what a mess, we should use something mutable or a monad
  let firstRow1 = pushBits start (allInfo `shiftR` (allInfoLength - fromIntegral amountFirstRow)) (fromIntegral amountFirstRow)
  let c1 = amountFirstRow
  let firstRow2 = pushBits firstRow1 0b1 1
  let secondRow1 = pushBits firstRow2 0b01 2
  let c2 = c1 + fromIntegral amountMiddleSecondRow
  let secondRow2 = pushBits secondRow1 (allInfo `shiftR` (allInfoLength - c2)) (fromIntegral amountMiddleSecondRow)
  let secondRow3 = pushBits secondRow2 0b0 1
  let c3 = c2 + 1
  let secondRow4 = pushBits secondRow3 (allInfo `shiftR` (allInfoLength - c3)) 1
  let otherRowsAmountOfBits = (matrixWidth - 4) * matrixWidth
  let c4 = c3 + (fromIntegral otherRowsAmountOfBits + 1)
  let otherRows = pushBits secondRow4 (allInfo `shiftR` (allInfoLength - c4)) (fromIntegral otherRowsAmountOfBits + 1)
  let secondLastRow1 = pushBits otherRows 0b0 1
  let c5 = c4 + fromIntegral amountMiddleSecondRow
  let secondLastRow2 = pushBits secondLastRow1 (allInfo `shiftR` (allInfoLength - c5)) (fromIntegral amountMiddleSecondRow)
  let secondLastRow3 = pushBits secondLastRow2 0b0 1
  let c6 = c5 + 1
  let secondLastRow4 = pushBits secondLastRow3 (allInfo `shiftR` (allInfoLength - c6)) 1
  let lastRow1 = pushBits secondLastRow4 0b1 1
  let amountLastRow = matrixWidth - 2
  let lastRow2 = pushBits lastRow1 (allInfo `shiftR` (allInfoLength - c6 - fromIntegral amountLastRow)) (fromIntegral amountLastRow)
  let lastRow3 = pushBits lastRow2 0b1 1
  let clearedPaddingOne =
        if amountOfZeroPadding > 0
          then clearBit lastRow3 (matrixWidth * matrixWidth - 3)
          else lastRow3
  (clearedPaddingOne, matrixWidth)

amountOfBitsForInteger :: Natural -> Int
amountOfBitsForInteger i = fromIntegral $ naturalLog2 i + 1

calcMatrixWidth :: Int -> Int
calcMatrixWidth amountOfBits = ceiling $ sqrt $ fromIntegral amountOfBits

pushBits :: Natural -> Natural -> Int -> Natural
pushBits n bits bitLength = n `shiftL` bitLength `xor` keepLSBits bits bitLength

keepLSBits :: Natural -> Int -> Natural
keepLSBits bits amount = bits .&. ones
  where
    ones :: Natural = 2 ^ amount - 1

crcShift :: Natural -> Natural
crcShift i = i `xor` (crcNumber `shift` (amountOfBitsForInteger i - amountOfBitsForInteger crcNumber))
  where
    crcNumber = 0o407

calculateCrc :: Natural -> Word8
calculateCrc i = do
  let shifted = i `shiftL` crcBitLength
  fromIntegral $ until (\z -> amountOfBitsForInteger z <= crcBitLength) crcShift shifted

removeBits :: Natural -> Int -> Int -> Natural
removeBits bits loc amount = (ms `shiftL` loc) `xor` ls
  where
    ms = bits `shiftR` (loc + amount)
    ls = keepLSBits bits loc

removeBits' :: Int -> Int -> Natural -> Natural
removeBits' loc amount bits = removeBits bits loc amount

-- todo: check if dotcode bits set correctly?
decode :: Natural -> Maybe Natural
decode dc =
  if calculateCrc number == crc
    then Just number
    else Nothing
  where
    numberAndCrc =
      dc & removeBits' topLeft 2
        & removeBits' topRight 1
        & removeBits' secondLeft 2
        & removeBits' secondRight 1
        & removeBits' secondLastLeft 1
        & removeBits' secondLastRight 1
        & removeBits' bottomLeft 1
        & removeBits' bottomRight 1
    number = numberAndCrc `shiftR` crcBitLength
    crc :: Word8 = fromIntegral $ keepLSBits numberAndCrc crcBitLength
    matrixWidth = calcMatrixWidth (amountOfBitsForInteger dc)
    topLeft = matrixWidth * matrixWidth - 2
    topRight = matrixWidth * (matrixWidth - 1)
    secondLeft = matrixWidth * (matrixWidth - 1) - 2
    secondRight = matrixWidth * (matrixWidth - 2) + 1
    secondLastLeft = matrixWidth * 2 - 2
    secondLastRight = matrixWidth + 1
    bottomLeft = matrixWidth - 1
    bottomRight = 0
