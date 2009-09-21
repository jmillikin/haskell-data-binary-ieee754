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
		[ props_GetFloat16be
		, props_GetFloat16le
		, props_GetFloat32be
		, props_GetFloat32le
		, props_GetFloat64be
		, props_GetFloat64le
		]
	runTests "passthrough" runDeep . map run $
		[ property $ passthrough putFloat32le getFloat32le
		, property $ passthrough putFloat32be getFloat32be
		, property $ passthrough putFloat64le getFloat64le
		, property $ passthrough putFloat64be getFloat64be
		]

props_GetFloat16be =
	[ valid [0, 0]      0.0
	, valid [0x80, 0] (-0.0)
	
	-- Normalized
	, valid [0x3C, 0]   1.0
	, valid [0xBC, 0] (-1.0)
	
	-- Denormalized
	, valid [0x03, 0xFF]   6.097555e-5
	, valid [0x83, 0xFF] (-6.097555e-5)
	
	-- Infinity
	, valid [0x7C, 0]   inf32
	, valid [0xFC, 0] (-inf32)
	
	-- NaN
	, nan [0x7E, 0]
	, nan [0xFE, 0]
	]
	where valid = checkValid getFloat16be
	      nan   = checkNaN   getFloat16be

props_GetFloat16le =
	[ valid [0, 0]      0.0
	, valid [0, 0x80] (-0.0)
	
	-- Normalized
	, valid [0, 0x3C]   1.0
	, valid [0, 0xBC] (-1.0)
	
	-- Denormalized
	, valid [0xFF, 0x03]   6.097555e-5
	, valid [0xFF, 0x83] (-6.097555e-5)
	
	-- Infinity
	, valid [0, 0x7C]   inf32
	, valid [0, 0xFC] (-inf32)
	
	-- NaN
	, nan [0, 0x7E]
	, nan [0, 0xFE]
	]
	where valid = checkValid getFloat16le
	      nan   = checkNaN   getFloat16le

props_GetFloat32be =
	[ valid [0, 0, 0, 0]      0.0
	, valid [0x80, 0, 0, 0] (-0.0)
	
	-- Normalized
	, valid [0x3F, 0x80, 0, 0]   1.0
	, valid [0xBF, 0x80, 0, 0] (-1.0)
	
	-- Denormalized
	, valid [0x00, 0x7F, 0xFF, 0xFF]   1.1754942106924411e-38
	, valid [0x80, 0x7F, 0xFF, 0xFF] (-1.1754942106924411e-38)
	
	-- Infinity
	, valid [0x7F, 0x80, 0, 0]   inf32
	, valid [0xFF, 0x80, 0, 0] (-inf32)
	
	-- NaN
	, nan [0x7F, 0xC0, 0, 0]
	, nan [0xFF, 0xC0, 0, 0]
	]
	where valid = checkValid getFloat32be
	      nan   = checkNaN   getFloat32be

props_GetFloat32le =
	[ valid [0, 0, 0, 0]      0.0
	, valid [0, 0, 0, 0x80] (-0.0)
	
	-- Normalized
	, valid [0, 0, 0x80, 0x3F]   1.0
	, valid [0, 0, 0x80, 0xBF] (-1.0)
	
	-- Denormalized
	, valid [0xFF, 0xFF, 0x7F, 0x00]   1.1754942106924411e-38
	, valid [0xFF, 0xFF, 0x7F, 0x80] (-1.1754942106924411e-38)
	
	-- Infinity
	, valid [0, 0, 0x80, 0x7F]   inf32
	, valid [0, 0, 0x80, 0xFF] (-inf32)
	
	-- NaN
	, nan [0, 0, 0xC0, 0x7F]
	, nan [0, 0, 0xC0, 0xFF]
	]
	where valid = checkValid getFloat32le
	      nan   = checkNaN   getFloat32le

props_GetFloat64be =
	[ valid [0, 0, 0, 0, 0, 0, 0, 0]   0.0
	, valid [0x80, 0, 0, 0, 0, 0, 0, 0] (-0.0)
	
	-- Normalized
	, valid [0x3F, 0xF0, 0, 0, 0, 0, 0, 0]   1.0
	, valid [0xBF, 0xF0, 0, 0, 0, 0, 0, 0] (-1.0)
	
	-- Denormalized
	, valid [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]   2.2250738585072009e-308
	, valid [0x80, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] (-2.2250738585072009e-308)
	
	-- Infinity
	, valid [0x7F, 0xF0, 0, 0, 0, 0, 0, 0]   inf64
	, valid [0xFF, 0xF0, 0, 0, 0, 0, 0, 0] (-inf64)
	
	-- NaN
	, nan [0x7F, 0xF8, 0, 0, 0, 0, 0, 0]
	, nan [0xFF, 0xF8, 0, 0, 0, 0, 0, 0]
	]
	where valid = checkValid getFloat64be
	      nan   = checkNaN   getFloat64be

props_GetFloat64le =
	[ valid [0, 0, 0, 0, 0, 0, 0, 0]      0.0
	, valid [0, 0, 0, 0, 0, 0, 0, 0x80] (-0.0)
	
	-- Normalized
	, valid [0, 0, 0, 0, 0, 0, 0xF0, 0x3F]   1.0
	, valid [0, 0, 0, 0, 0, 0, 0xF0, 0xBF] (-1.0)
	
	-- Denormalized
	, valid [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00]   2.2250738585072009e-308
	, valid [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x80] (-2.2250738585072009e-308)
	
	-- Infinity
	, valid [0, 0, 0, 0, 0, 0, 0xF0, 0x7F]   inf64
	, valid [0, 0, 0, 0, 0, 0, 0xF0, 0xFF] (-inf64)
	
	-- NaN
	, nan [0, 0, 0, 0, 0, 0, 0xF8, 0x7F]
	, nan [0, 0, 0, 0, 0, 0, 0xF8, 0xFF]
	]
	where valid = checkValid getFloat64le
	      nan   = checkNaN   getFloat64le

checkValid :: Eq a => Get a -> [Word8] -> a -> Bool
checkValid get words x = x == x' && B.null remaining where
	(x', remaining, _) = runGetState get (B.pack words) 0

checkNaN :: RealFloat a => Get a -> [Word8] -> Bool
checkNaN get words = isNaN x' && B.null remaining where
	(x', remaining, _) = runGetState get (B.pack words) 0

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

nan64 :: Float
nan64 = read "NaN"
