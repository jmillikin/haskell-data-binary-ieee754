{- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main () where

import Test.HUnit
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as LB
import Data.Binary.Get (runGetState)
import Data.Binary.Put (runPut)

allTests = "allTests" ~: TestList [getTests, putTests]

getTests = "getTests" ~: TestList [
	 exponentWidthTests
	,bitSliceTests
	,splitRawIEEE754Tests
	,unbiasTests
	,mergeFloatTests
	,parseFloatBETests
	,parseFloatLETests
	,getFloatTests
	]

putTests = "putTests" ~: TestList [
	 putFloatTests
	,floatComponentsTests
	,biasTests
	,encodeIntBETests
	,encodeIntLETests
	,floatToMergedTests
	,mergeFloatBitsTests
	]

exponentWidthTests = "wTests" ~: TestList [
	 testexponentWidth 16  5
	,testexponentWidth 32  8
	,testexponentWidth 64  11
	,testexponentWidth 128 15
	]
		where testexponentWidth v e = e ~=? (exponentWidth v)

bitSliceTests = "bitSliceTests" ~: TestList [
	 testBitSlice ([],           0, 0)   0
	,testBitSlice ([0xFF],       0, 0)   0
	,testBitSlice ([0xFF],       0, 1)   1
	,testBitSlice ([0xFF],       1, 2)   1
	,testBitSlice ([0xFF],       0, 2)   3
	,testBitSlice ([0x00, 0xFF], 9, 11)  3
	,testBitSlice ([0xFF],       1, 3)   3
	,testBitSlice ([0x02, 0xa0], 6, 12) 42
	,testBitSlice ([0x02, 0xa0], 6, 12) 42
	,testBitSlice ([0xFF, 0xF0, 0, 0], 1, 12) 2047
	]
		where testBitSlice (ws, begin, end) expected = expected ~=? (bitSlice ws begin end)

splitRawIEEE754Tests = "splitRawIEEE754Tests" ~: TestList [
	-- Zero
	 testSplit [0, 0, 0, 0] (False, 0, 0)
	 
	-- Negative zero
	,testSplit [0x80, 0, 0, 0] (True, 0, 0)
	
	-- All-1s exponent (Infinity)
	,testSplit [0x7F, 0x80, 0, 0] (False, 255, 0)
	
	-- All-1s exponent, negative (-Infinity)
	,testSplit [0xFF, 0x80, 0, 0] (True, 255, 0)
	
	-- All-1s fraction
	,testSplit [0, 0x7F, 0xFF, 0xFF] (False, 0, 8388607)
	
	-- 64-bit Zero
	,testSplit [0, 0, 0, 0, 0, 0, 0, 0] (False, 0, 0)
	
	-- 64-bit Negative Zero
	,testSplit [0x80, 0, 0, 0, 0, 0, 0, 0] (True, 0, 0)
	
	-- 64-bit All-1s exponent (Infinity)
	,testSplit [0x7F, 0xF0, 0, 0, 0, 0, 0, 0] (False, 2047, 0)
	
	-- 64-bit All-1s exponent, negative (-Infinity)
	,testSplit [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (True, 2047, 0)
	
	-- 64-bit All-1s fraction
	,testSplit [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (False, 0, 4503599627370495)
	]
		where testSplit ws expected = expected ~=? (splitRawIEEE754 ws)

unbiasTests = "unbiasTests" ~: TestList [
	-- -127
	 testUnbias (0, 8) (-127)
	
	-- Zero
	,testUnbias (127, 8) 0
	
	-- 127
	,testUnbias (254, 8) 127
	
	-- 128 (Infinity/NaN)
	,testUnbias (255, 8) 128
	]
		where testUnbias (e, eWidth) expected = expected ~=? (unbias e eWidth)

mergeFloatTests = "mergeFloatTests" ~: TestList [
	 testMerge (0, 0, 32) (0, 0)
	,testMerge (127, 0, 32) (8388608, -23)
	]
		where testMerge (e, eMax, f) expected = expected ~=? (mergeFloat e eMax f)

parseFloatBETests = "parseFloat32beTests" ~: TestList [
	-- Zero
	 testParse [0, 0, 0, 0] (0.0 :: Float)
	,testParse [0, 0, 0, 0, 0, 0, 0, 0] (0.0 :: Double)
	
	-- Negative zero
	,testParse [0x80, 0, 0, 0] (-0.0 :: Float)
	,testParse [0x80, 0, 0, 0, 0, 0, 0, 0] (-0.0 :: Double)
	
	-- Normalized
	,testParse [0x3F, 0x80, 0, 0] (1.0 :: Float)
	,testParse [0xBF, 0x80, 0, 0] (-1.0 :: Float)
	
	,testParse [0x3F, 0xF0, 0, 0, 0, 0, 0, 0] (1 :: Double)
	,testParse [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (-1 :: Double)
	
	-- Denormalized
	,testParse [0x00, 0x7F, 0xFF, 0xFF] ( 1.1754942106924411e-38 :: Float)
	,testParse [0x80, 0x7F, 0xFF, 0xFF] (-1.1754942106924411e-38 :: Float)
	
	,testParse [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] ( 2.2250738585072009e-308 :: Double)
	,testParse [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (-2.2250738585072009e-308 :: Double)
	
	-- Infinity
	
	-- Negative Infinity
	
	-- NaN
	
	-- Negative NaN
	]
		where testParse ws expected = expected ~=? (parseFloatBE ws)

parseFloatLETests = "parseFloat32beTests" ~: TestList [
	-- Zero
	 testParse [0, 0, 0, 0] (0.0 :: Float)
	,testParse [0, 0, 0, 0, 0, 0, 0, 0] (0.0 :: Double)
	
	-- Negative zero
	,testParse [0, 0, 0, 0x80] (-0.0 :: Float)
	,testParse [0, 0, 0, 0, 0, 0, 0, 0x80] (-0.0 :: Double)
	
	-- Normalized
	,testParse [0, 0, 0x80, 0x3F] (1.0 :: Float)
	,testParse [0, 0, 0x80, 0xBF] (-1.0 :: Float)
	
	,testParse [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] (1.0 :: Double)
	,testParse [0, 0, 0, 0, 0, 0, 0xF0, 0xBF] (-1.0 :: Double)
	
	-- Denormalized
	,testParse [0xFF, 0xFF, 0x7F, 0x00] ( 1.1754942106924411e-38 :: Float)
	,testParse [0xFF, 0xFF, 0x7F, 0x80] (-1.1754942106924411e-38 :: Float)
	
	,testParse [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00] ( 2.2250738585072009e-308 :: Double)
	,testParse [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x80] (-2.2250738585072009e-308 :: Double)
	
	-- TODO: Infinity
	
	-- TODO: Negative Infinity
	
	-- TODO: NaN
	
	-- TODO: Negative NaN
	
	]
		where testParse ws expected = expected ~=? (parseFloatLE ws)

getFloatTests = "getFloatTests" ~: TestList [
	 testGetFloat getFloat16be [0, 0] (0.0 :: Float)
	,testGetFloat getFloat16be [0x80, 0] (-0.0 :: Float)
	,testGetFloat getFloat16le [0, 0] (0.0 :: Float)
	,testGetFloat getFloat16le [0, 0x80] (-0.0 :: Float)
	
	,testGetFloat getFloat32be [0, 0, 0, 0] (0.0 :: Float)
	,testGetFloat getFloat32be [0x80, 0, 0, 0] (-0.0 :: Float)
	,testGetFloat getFloat32le [0, 0, 0, 0] (0.0 :: Float)
	,testGetFloat getFloat32le [0, 0, 0, 0x80] (-0.0 :: Float)
	
	,testGetFloat getFloat64be [0, 0, 0, 0, 0, 0, 0, 0] (0.0 :: Double)
	,testGetFloat getFloat64be [0x80, 0, 0, 0, 0, 0, 0, 0] (-0.0 :: Double)
	,testGetFloat getFloat64le [0, 0, 0, 0, 0, 0, 0, 0] (0.0 :: Double)
	,testGetFloat getFloat64le [0, 0, 0, 0, 0, 0, 0, 0x80] (-0.0 :: Double)
	]
		where testGetFloat f ws expected = fullExpected ~=? result
			where fullExpected = (expected, LB.empty, (fromIntegral (length ws)))
			      result       = runGetState f (LB.pack ws) (fromIntegral 0)

putFloatTests = "putFloatTests" ~: TestList [
	-- 0 and -0
	 testPutFloat putFloat32be ( 0.0 :: Float) [0, 0, 0, 0]
	,testPutFloat putFloat32be (-0.0 :: Float) [0x80, 0, 0, 0]
	,testPutFloat putFloat32le ( 0.0 :: Float) [0, 0, 0, 0]
	,testPutFloat putFloat32le (-0.0 :: Float) [0, 0, 0, 0x80]
	
	,testPutFloat putFloat64be ( 0.0 :: Double) [0, 0, 0, 0, 0, 0, 0, 0]
	,testPutFloat putFloat64be (-0.0 :: Double) [0x80, 0, 0, 0, 0, 0, 0, 0]
	,testPutFloat putFloat64le ( 0.0 :: Double) [0, 0, 0, 0, 0, 0, 0, 0]
	,testPutFloat putFloat64le (-0.0 :: Double) [0, 0, 0, 0, 0, 0, 0, 0x80]
	
	-- 1 and -1
	,testPutFloat putFloat32be ( 1.0 :: Float) [0x3F, 0x80, 0, 0]
	,testPutFloat putFloat32be (-1.0 :: Float) [0xBF, 0x80, 0, 0]
	,testPutFloat putFloat32le ( 1.0 :: Float) [0, 0, 0x80, 0x3F]
	,testPutFloat putFloat32le (-1.0 :: Float) [0, 0, 0x80, 0xBF]
	
	,testPutFloat putFloat64be ( 1.0 :: Double) [0x3F, 0xF0, 0, 0, 0, 0, 0, 0]
	,testPutFloat putFloat64be (-1.0 :: Double) [0xBF, 0xF0, 0, 0, 0, 0, 0, 0]
	,testPutFloat putFloat64le ( 1.0 :: Double) [0, 0, 0, 0, 0, 0, 0xF0, 0x3F]
	,testPutFloat putFloat64le (-1.0 :: Double) [0, 0, 0, 0, 0, 0, 0xF0, 0xBF]
	
	-- Denormalized
	,testPutFloat putFloat32be (1.1754942106924411e-38 :: Float) [0, 0x7F, 0xFF, 0xFF]
	,testPutFloat putFloat32le (1.1754942106924411e-38 :: Float) [0xFF, 0xFF, 0x7F, 0]
	
	,testPutFloat putFloat32be (-1.1754942106924411e-38 :: Float) [0x80, 0x7F, 0xFF, 0xFF]
	,testPutFloat putFloat32le (-1.1754942106924411e-38 :: Float) [0xFF, 0xFF, 0x7F, 0x80]
	
	,testPutFloat putFloat64be (2.2250738585072009e-308 :: Double) [0, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0FF, 0xFF]
	,testPutFloat putFloat64le (2.2250738585072009e-308 :: Double) [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0]
	
	,testPutFloat putFloat64be (-2.2250738585072009e-308 :: Double) [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0FF, 0xFF]
	,testPutFloat putFloat64le (-2.2250738585072009e-308 :: Double) [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x80]
	]
		where testPutFloat f x expected = expected ~=? result
			where result = LB.unpack . runPut $ f x

floatComponentsTests = "floatComponentsTests" ~: TestList [
	-- 0 and -0
	 testComponents 4 ( 0.0 :: Float) (False, 0, 0)
	,testComponents 4 (-0.0 :: Float) (True, 0, 0)
	
	,testComponents 8 ( 0.0 :: Double) (False, 0, 0)
	,testComponents 8 (-0.0 :: Double) (True, 0, 0)
	
	-- 1 and -1
	,testComponents 4 ( 1.0 :: Float) (False, 0, 127)
	,testComponents 4 (-1.0 :: Float) (True, 0, 127)
	
	,testComponents 8 ( 1.0 :: Double) (False, 0, 1023)
	,testComponents 8 (-1.0 :: Double) (True, 0, 1023)
	
	-- Denormalized
	,testComponents 4 (1.1754942106924411e-38 :: Float) (False, 8388607, 0)
	,testComponents 8 (2.2250738585072009e-308 :: Double) (False, 4503599627370495, 0)
	
	]
		where testComponents width x expected = expected ~=? result
			where result = floatComponents width x

biasTests = "biasTests" ~: TestList [
	 testBias (-127, 8) 0
	,testBias (0, 8) 127
	,testBias (127, 8) 254
	,testBias (128, 8) 255
	]
		where testBias (e, eWidth) expected = expected ~=? (bias e eWidth)

encodeIntBETests = "encodeIntBETests" ~: TestList [
	 testEncode 1 0 [0]
	,testEncode 4 0 [0, 0, 0, 0]
	,testEncode 4 1 [0, 0, 0, 0x01]
	,testEncode 4 300 [0, 0, 0x01, 0x2C]
	]
		where testEncode width x expected = expected ~=? (encodeIntBE width x)

encodeIntLETests = "encodeIntLETests" ~: TestList [
	 testEncode 1 0 [0]
	,testEncode 4 0 [0, 0, 0, 0]
	,testEncode 4 1 [0x01, 0, 0, 0]
	,testEncode 4 300 [0x2C, 0x01, 0, 0]
	]
		where testEncode width x expected = expected ~=? (encodeIntLE width x)

floatToMergedTests = "floatToWordsTests" ~: TestList [
	 testToMerged 4 0.0 0
	]
		where testToMerged width v expected = expected ~=? (floatToMerged width v)

mergeFloatBitsTests = "mergeFloatBitsTests" ~: TestList [
	 testMerge 2 3 (False, 0, 0)    0
	,testMerge 2 3 (True,  0, 0) 0x20
	
	,testMerge 2 3 (False, 2, 3) 0x0E
	,testMerge 2 3 (True,  2, 3) 0x2E
	]
		where testMerge fWidth eWidth (s, f, e) expected = expected ~=? (mergeFloatBits fWidth eWidth s f e)

main = do
	runTestTT allTests
