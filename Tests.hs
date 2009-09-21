{-
  Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
  
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

import Test.QuickCheck
import Test.QuickCheck.Batch

import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)
import Data.Binary.Get (Get, runGetState)
import Data.Binary.Put (Put, runPut)

import Data.Binary.IEEE754

runOnce = TestOptions
	{ no_of_tests     = 1
	, length_of_tests = 1
	, debug_tests     = False
	}

runDeep = TestOptions
	{ no_of_tests     = 1000
	, length_of_tests = 0
	, debug_tests     = False
	}

main = do
	runTests "parsing" runOnce . map (run . property) . concat $
		[ props_GetFloat16
		, props_GetFloat32
		, props_GetFloat64
		]
	runTests "serialising" runOnce . map (run . property) . concat $
		[ props_PutFloat32
		, props_PutFloat64
		]
	runTests "passthrough" runDeep . map run $
		[ property $ passthrough putFloat32le getFloat32le
		, property $ passthrough putFloat32be getFloat32be
		, property $ passthrough putFloat64le getFloat64le
		, property $ passthrough putFloat64be getFloat64be
		]

props_GetFloat16 = let check = checkGet getFloat16be getFloat16le in
	[ check [0, 0]    (and' (== 0.0) (not . isNegativeZero))
	, check [0x80, 0] isNegativeZero
	
	-- Normalized
	, check [0x3C, 0] (==  1.0)
	, check [0xBC, 0] (== -1.0)
	
	-- Denormalized
	, check [0x03, 0xFF] (==  6.097555e-5)
	, check [0x83, 0xFF] (== -6.097555e-5)
	
	-- Infinity
	, check [0x7C, 0] (==  inf32)
	, check [0xFC, 0] (== -inf32)
	
	-- NaN
	, check [0x7E, 0] isNaN
	, check [0xFE, 0] isNaN
	]

props_GetFloat32 = let check = checkGet getFloat32be getFloat32le in
	[ check [0, 0, 0, 0]    (and' (== 0.0) (not . isNegativeZero))
	, check [0x80, 0, 0, 0] isNegativeZero
	
	-- Normalized
	, check [0x3F, 0x80, 0, 0] (==  1.0)
	, check [0xBF, 0x80, 0, 0] (== -1.0)
	
	-- Denormalized
	, check [0x00, 0x7F, 0xFF, 0xFF] (==  1.1754942106924411e-38)
	, check [0x80, 0x7F, 0xFF, 0xFF] (== -1.1754942106924411e-38)
	
	-- Infinity
	, check [0x7F, 0x80, 0, 0] (==  inf32)
	, check [0xFF, 0x80, 0, 0] (== -inf32)
	
	-- NaN and negative NaN
	, check [0x7F, 0xC0, 0, 0] isNaN
	, check [0xFF, 0xC0, 0, 0] isNaN
	]

props_GetFloat64 = let check = checkGet getFloat64be getFloat64le in
	[ check [0, 0, 0, 0, 0, 0, 0, 0]    (and' (== 0.0) (not . isNegativeZero))
	, check [0x80, 0, 0, 0, 0, 0, 0, 0] isNegativeZero
	
	-- Normalized
	, check [0x3F, 0xF0, 0, 0, 0, 0, 0, 0] (==  1.0)
	, check [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (== -1.0)
	
	-- Denormalized
	, check [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (==  2.2250738585072009e-308)
	, check [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (== -2.2250738585072009e-308)
	
	-- Infinity
	, check [0x7F, 0xF0, 0, 0, 0, 0, 0, 0] (==  inf64)
	, check [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (== -inf64)
	
	-- NaN
	, check [0x7F, 0xF8, 0, 0, 0, 0, 0, 0] (and' isNaN (not . isNegativeNaN))
	, check [0xFF, 0xF8, 0, 0, 0, 0, 0, 0] isNegativeNaN
	]

props_PutFloat32 = let check = checkPut putFloat32be putFloat32le in
	[ check [0, 0, 0, 0]   0.0
	, check [0x80, 0, 0, 0] (-0.0)
	
	-- Normalized
	, check [0x3F, 0x80, 0, 0]   1.0
	, check [0xBF, 0x80, 0, 0] (-1.0)
	
	-- Denormalized
	, check [0x00, 0x7F, 0xFF, 0xFF]   1.1754942106924411e-38
	, check [0x80, 0x7F, 0xFF, 0xFF] (-1.1754942106924411e-38)
	
	-- Infinity
	, check [0x7F, 0x80, 0, 0]   inf32
	, check [0x7F, 0x80, 0, 0] (-inf32)
	
	-- NaN
	, check [0x7F, 0xC0, 0, 0]   nan32
	, check [0xFF, 0xC0, 0, 0] (-nan32)
	]

props_PutFloat64 = let check = checkPut putFloat64be putFloat64le in
	[ check [0, 0, 0, 0, 0, 0, 0, 0]      0.0
	, check [0x80, 0, 0, 0, 0, 0, 0, 0] (-0.0)
	
	-- Normalized
	, check [0x3F, 0xF0, 0, 0, 0, 0, 0, 0]   1.0
	, check [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (-1.0)
	
	-- Denormalized
	, check [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]   2.2250738585072009e-308
	, check [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (-2.2250738585072009e-308)
	
	-- Infinity
	, check [0x7F, 0xF0, 0, 0, 0, 0, 0, 0]   inf64
	, check [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (-inf64)
	
	-- NaN
	, check [0x7F, 0xF8, 0, 0, 0, 0, 0, 0]   nan64
	, check [0xFF, 0xF8, 0, 0, 0, 0, 0, 0] (-nan64)
	]

checkGet :: Eq a => Get a -> Get a -> [Word8] -> (a -> Bool) -> Bool
checkGet getBE getLE bytes f = valid where
	valid = sameResult && B.null remainingBE && f xBE
	sameResult = remainingBE == remainingLE && xBE == xLE
	(xBE, remainingBE, _) = runGetState getBE (B.pack bytes) 0
	(xLE, remainingLE, _) = runGetState getLE (B.pack (reverse bytes)) 0

checkPut :: (a -> Put) -> (a -> Put) -> [Word8] -> a -> Bool
checkPut putBE putLE bytes x = valid where
	valid = sameResult && bytes == B.unpack bytesBE
	sameResult = bytesBE == (B.reverse bytesLE)
	bytesBE = runPut (putBE x)
	bytesLE = runPut (putLE x)

and' :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and' f g x = f x && g x

isNegativeNaN :: RealFloat a => a -> Bool
isNegativeNaN x = isNaN x && (floor x > 0)

-- Verify that the given put and get functions 
passthrough :: Eq a => (a -> Put) -> Get a -> a -> Bool
passthrough put get x = x == x' && B.null remaining where
	bytes = runPut (put x)
	(x', remaining, _) = runGetState get bytes 0

-- Pseudo-literals for special values
inf32 :: Float
inf32 = read "Infinity"

inf64 :: Double
inf64 = read "Infinity"

nan32 :: Float
nan32 = read "NaN"

nan64 :: Double
nan64 = read "NaN"
