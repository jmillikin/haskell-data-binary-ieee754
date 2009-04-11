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

module Data.Binary.IEEE754 (
	 parseFloatBE, parseFloatLE
	
	,getFloat16be, getFloat16le
	,getFloat32be, getFloat32le
	,getFloat64be, getFloat64le
	
	,getFloat
	,exponentWidth
	,bitSlice
	,splitRawIEEE754
	,unbias
	,mergeFloat
) where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.Word (Word8)
import Data.List (foldl')

import qualified Data.ByteString as B
import Data.Binary.Get (Get, getByteString)

parseFloatBE :: (RealFloat a) => [Word8] -> a
parseFloatBE = parseFloat

parseFloatLE :: (RealFloat a) => [Word8] -> a
parseFloatLE = parseFloat . reverse

getFloat16be :: Get Float
getFloat16be = getFloat 16 parseFloatBE

getFloat16le :: Get Float
getFloat16le = getFloat 16 parseFloatLE

getFloat32be :: Get Float
getFloat32be = getFloat 32 parseFloatBE

getFloat32le :: Get Float
getFloat32le = getFloat 32 parseFloatLE

getFloat64be :: Get Double
getFloat64be = getFloat 64 parseFloatBE

getFloat64le :: Get Double
getFloat64le = getFloat 64 parseFloatLE

type Exponent = Int
type Fraction = Integer
type BitCount = Int

parseFloat :: (RealFloat a) => [Word8] -> a
parseFloat bs = merge' (splitRawIEEE754 bs)
	where merge'  (sign, e, f) = encode' (mergeFloat e f width) * signFactor sign
	      encode' (f, e)       = encodeFloat f e
	      signFactor s         = if s then (-1) else 1
	      width                = length bs * 8

getFloat :: (RealFloat a) => BitCount -> ([Word8] -> a) -> Get a
getFloat width parser = do
	bytes <- getByteString width
	(return . parser . B.unpack) bytes

-- Calculate the proper size of the exponent field, in bits, given the
-- size of the full structure.
exponentWidth :: BitCount -> BitCount
exponentWidth k
	| k == 16         = 5
	| k == 32         = 8
	| k `mod` 32 == 0 = ceiling (4 * (log2 k)) - 13
	| otherwise       = error "Invalid length of floating-point value"

-- Considering a byte list as a sequence of bits, slice it from start inclusive
-- to end exclusive, and return the resulting bit sequence as an integer
bitSlice :: [Word8] -> BitCount -> BitCount -> Integer
bitSlice bs = sliceInt (foldl' step 0 bs) bitCount
	where step acc w     = (shiftL acc 8) + (fromIntegral w)
	      bitCount       = ((length bs) * 8)

-- Slice a single integer by start and end bit location
sliceInt :: Integer -> BitCount -> BitCount -> BitCount -> Integer
sliceInt x xBitCount s e = fromIntegral $ (x .&. startMask) `shiftR` (xBitCount - e)
	where startMask = n1Bits (xBitCount - s)
	      n1Bits n  = (2 `iExp` n) - 1

-- Split a raw bit array into (sign, exponent, fraction) components. These
-- components have not been processed (unbiased, added significant bit, etc).
splitRawIEEE754 :: [Word8] -> (Bool, Exponent, Fraction)
splitRawIEEE754 bs = (sign, exp, frac)
	where sign = (head bs .&. 0x80) == 0x80
	      exp  = fromIntegral $ bitSlice bs 1 (1 + w)
	      frac = bitSlice bs (1 + w) (length bs * 8)
	      w    = exponentWidth $ length bs * 8

-- Unbias an exponent
unbias :: Exponent -> BitCount -> Exponent
unbias e eWidth = e + 1 - (2 `iExp` (eWidth - 1))

-- Parse values into a form suitable for encodeFloat
-- sign exponent fraction width-in-bits -> fraction, exponent
mergeFloat :: Exponent -> Fraction -> BitCount -> (Integer, Int)

-- Zero
mergeFloat 0 0 _ = (0, 0)

mergeFloat e f width
	-- Infinity / NaN (TODO
	| e == eMax = error "Infinity/NaN not supported"
	
	| otherwise = case e of
		-- Denormalized
		0 -> (f, (-fWidth) + (unbiasedE + 1))
		
		-- Normalized
		_ -> (f + (1 `shiftL` fWidth), (-fWidth) + unbiasedE)
		
		where eWidth    = exponentWidth width
		      fWidth    = width - eWidth - 1
		      eMax      = (2 `iExp` eWidth) - 1
		      unbiasedE = unbias e (eWidth)

-- Base-2 log of an integer
log2 = (logBase 2) . fromIntegral

-- Integral exponent
iExp b e = floor $ (fromIntegral b) ** (fromIntegral e)
