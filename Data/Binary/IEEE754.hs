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
	-- * Parsing
	 parseFloatBE, parseFloatLE
	
	,getFloat16be, getFloat16le
	,getFloat32be, getFloat32le
	,getFloat64be, getFloat64le
	
	,getFloat
	
	-- * Serializing
	,putFloat32be, putFloat32le
	,putFloat64be, putFloat64le
	
	,putFloat
	
	-- * Parser implementation
	,exponentWidth
	,bitSlice
	,splitRawIEEE754
	,unbias
	,mergeFloat
	
	-- * Serializer implementation
	,bias
	,encodeIntBE, encodeIntLE
	,floatToMerged
	,mergeFloatBits
	,floatComponents
	
	-- * Useful type aliases
	,Exponent
	,Fraction
	,BitCount
) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8)
import Data.List (foldl')

import qualified Data.ByteString as B
import Data.Binary.Get (Get, getByteString)
import Data.Binary.Put (Put, putByteString)

---------------------------------------------------------------------

-- |Parse a big-endian byte list into a floating-point value.
parseFloatBE :: (RealFloat a) => [Word8] -> a
parseFloatBE = parseFloat

-- |Parse a little-endian byte list into a floating-point value.
parseFloatLE :: (RealFloat a) => [Word8] -> a
parseFloatLE = parseFloat . reverse

getFloat16be :: Get Float
getFloat16be = getFloat 2 parseFloatBE

getFloat16le :: Get Float
getFloat16le = getFloat 2 parseFloatLE

getFloat32be :: Get Float
getFloat32be = getFloat 4 parseFloatBE

getFloat32le :: Get Float
getFloat32le = getFloat 4 parseFloatLE

getFloat64be :: Get Double
getFloat64be = getFloat 8 parseFloatBE

getFloat64le :: Get Double
getFloat64le = getFloat 8 parseFloatLE

-- |Parse a floating-point value of the given width (in bytes) from within
-- a Get monad.
getFloat :: (RealFloat a) => ByteCount -> ([Word8] -> a) -> Get a
getFloat width parser = do
	bytes <- getByteString width
	(return . parser . B.unpack) bytes

---------------------------------------------------------------------

putFloat32be :: (RealFloat a) => a -> Put
putFloat32be = putFloat 4 encodeIntBE

putFloat32le :: (RealFloat a) => a -> Put
putFloat32le = putFloat 4 encodeIntLE

putFloat64be :: (RealFloat a) => a -> Put
putFloat64be = putFloat 8 encodeIntBE

putFloat64le :: (RealFloat a) => a -> Put
putFloat64le = putFloat 8 encodeIntLE

putFloat :: (RealFloat a) => ByteCount -> (ByteCount -> Integer -> [Word8]) -> a -> Put
putFloat width f v = putByteString $ B.pack words
	where words = f width (floatToMerged width v)

floatComponents :: (RealFloat a) => ByteCount -> a -> (Bool, Fraction, Exponent)
floatComponents width v =
	case (dFraction, dExponent, biasedE) of
		(0, 0, _) -> (sign, 0, 0)
		(_, _, 0) -> (sign, truncatedFraction + 1, 0)
		otherwise -> (sign, truncatedFraction, biasedE)
	where dFraction   = fst (decodeFloat v)
	      dExponent   = snd (decodeFloat v)
	      eWidth      = exponentWidth (width * 8)
	      fWidth      = (width * 8) - eWidth - 1 -- 1 for sign bit
	      biasedE     = bias (dExponent + fWidth) eWidth
	      absFraction = abs dFraction
	
	      -- Weird check is for detecting -0.0
	      sign        = (1.0 / v) < 0.0
	
	      -- Fraction needs to be truncated, depending on the exponent
	      truncatedFraction = absFraction - (1 `shiftL` fWidth)

floatToMerged :: (RealFloat a) => ByteCount -> a -> Integer
floatToMerged width v = mergeFloatBits' (floatComponents width v)
	where mergeFloatBits' (s, f, e) = mergeFloatBits fWidth eWidth s f e
	      eWidth      = exponentWidth (width * 8)
	      fWidth      = (width * 8) - eWidth - 1 -- 1 for sign bit

mergeFloatBits :: BitCount -> BitCount -> Bool -> Fraction -> Exponent -> Integer
mergeFloatBits fWidth eWidth s f e = shiftedSign .|. shiftedFrac .|. shiftedExp
	where sBit = (if s then 1 else 0) :: Integer
	      shiftedSign = (sBit `shiftL` (fWidth + eWidth)) :: Integer
	      shiftedExp  = ((fromIntegral e) `shiftL` fWidth) :: Integer
	      shiftedFrac = f

-- |Encode an integer to a list of words, in big-endian format
encodeIntBE :: ByteCount -> Integer -> [Word8]
encodeIntBE 0     x = []
encodeIntBE width x = (encodeIntBE (width - 1) (x `shiftR` 8)) ++ [step]
	where step = (fromIntegral x) .&. 0xFF

-- |Encode an integer to a list of words, in little-endian format
encodeIntLE :: ByteCount -> Integer -> [Word8]
encodeIntLE width x = reverse (encodeIntBE width x)

bias e eWidth = e - (1 - (2 `iExp` (eWidth - 1)))

---------------------------------------------------------------------

parseFloat :: (RealFloat a) => [Word8] -> a
parseFloat bs = merge' (splitRawIEEE754 bs)
	where merge'  (sign, e, f) = encode' (mergeFloat e f width) * signFactor sign
	      encode' (f, e)       = encodeFloat f e
	      signFactor s         = if s then (-1) else 1
	      width                = length bs * 8

-- |Considering a byte list as a sequence of bits, slice it from start
-- inclusive to end exclusive, and return the resulting bit sequence as an
-- integer
bitSlice :: [Word8] -> BitCount -> BitCount -> Integer
bitSlice bs = sliceInt (foldl' step 0 bs) bitCount
	where step acc w     = (shiftL acc 8) + (fromIntegral w)
	      bitCount       = ((length bs) * 8)

-- |Slice a single integer by start and end bit location
sliceInt :: Integer -> BitCount -> BitCount -> BitCount -> Integer
sliceInt x xBitCount s e = fromIntegral $ (x .&. startMask) `shiftR` (xBitCount - e)
	where startMask = n1Bits (xBitCount - s)
	      n1Bits n  = (2 `iExp` n) - 1

-- |Split a raw bit array into (sign, exponent, fraction) components. These
-- components have not been processed (unbiased, added significant bit,
-- etc).
splitRawIEEE754 :: [Word8] -> (Bool, Exponent, Fraction)
splitRawIEEE754 bs = (sign, exp, frac)
	where sign = (head bs .&. 0x80) == 0x80
	      exp  = fromIntegral $ bitSlice bs 1 (1 + w)
	      frac = bitSlice bs (1 + w) (length bs * 8)
	      w    = exponentWidth $ length bs * 8

-- |Unbias an exponent
unbias :: Exponent -> BitCount -> Exponent
unbias e eWidth = e + 1 - (2 `iExp` (eWidth - 1))

-- |Parse values into a form suitable for encodeFloat
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

---------------------------------------------------------------------

-- |Calculate the proper size of the exponent field, in bits, given the
-- size of the full structure.
exponentWidth :: BitCount -> BitCount
exponentWidth k
	| k == 16         = 5
	| k == 32         = 8
	| k `mod` 32 == 0 = ceiling (4 * (log2 k)) - 13
	| otherwise       = error "Invalid length of floating-point value"

-- |Base-2 log of an integer
log2 = (logBase 2) . fromIntegral

-- |Integral exponent
iExp b e = floor $ (fromIntegral b) ** (fromIntegral e)

type Exponent = Int
type Fraction = Integer
type BitCount = Int
type ByteCount = Int
