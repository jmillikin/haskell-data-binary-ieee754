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

allTests = "allTests" ~: TestList [
	 exponentWidthTests
	,bitSliceTests
	,splitRawIEEE754Tests
	,unbiasTests
	,mergeFloatTests
	,parseFloatBETests
	,parseFloatLETests
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
	
	-- Infinity
	
	-- Negative Infinity
	
	-- NaN
	
	-- Negative NaN
	
	]
		where testParse ws expected = expected ~=? (parseFloatLE ws)

main = do
	runTestTT allTests
