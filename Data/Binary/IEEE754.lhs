% Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\ignore{
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Binary.IEEE754 (
	-- * Parsing
	  getFloat16be, getFloat16le
	, getFloat32be, getFloat32le
	, getFloat64be, getFloat64le
	
	-- * Serializing
	, putFloat32be, putFloat32le
	, putFloat64be, putFloat64le
) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, Bits)
import Data.Word (Word8)
import Data.List (foldl')

import qualified Data.ByteString as B
import Data.Binary.Get (Get, getByteString)
import Data.Binary.Put (Put, putByteString)
\end{code}
}

\section{Parsing}

\subsection{Public interface}

\begin{code}
getFloat16be :: Get Float
getFloat16be = getFloat (ByteCount 2) parseFloat
\end{code}

\begin{code}
getFloat16le :: Get Float
getFloat16le = getFloat (ByteCount 2) $ parseFloat . reverse
\end{code}

\begin{code}
getFloat32be :: Get Float
getFloat32be = getFloat (ByteCount 4) parseFloat
\end{code}

\begin{code}
getFloat32le :: Get Float
getFloat32le = getFloat (ByteCount 4) $ parseFloat . reverse
\end{code}

\begin{code}
getFloat64be :: Get Double
getFloat64be = getFloat (ByteCount 8) parseFloat
\end{code}

\begin{code}
getFloat64le :: Get Double
getFloat64le = getFloat (ByteCount 8) $ parseFloat . reverse
\end{code}

\subsection{Implementation}

Parse a floating-point value of the given width (in bytes) from within
a {\tt Get} monad.
\begin{code}
getFloat :: RealFloat a => ByteCount -> ([Word8] -> a) -> Get a
getFloat (ByteCount width) parser = fmap (parser . B.unpack) $ getByteString width
\end{code}

\begin{code}
parseFloat :: RealFloat a => [Word8] -> a
parseFloat bs = merge' $ splitRawIEEE754 bs where
	merge'  (sign, e, f) = encode' (mergeFloat e f width) * signFactor sign
	encode' (f, e)       = encodeFloat f e
	signFactor s         = if s then (-1) else 1
	width                = bitsInWord8 bs
\end{code}

Considering a byte list as a sequence of bits, slice it from start
inclusive to end exclusive, and return the resulting bit sequence as an
integer.

\begin{code}
bitSlice :: [Word8] -> BitCount -> BitCount -> Integer
bitSlice bs = sliceInt (foldl' step 0 bs) bitCount' where
	step acc w     = shiftL acc 8 + fromIntegral w
	bitCount'      = bitsInWord8 bs
\end{code}

Slice a single integer by start and end bit location

\begin{code}
sliceInt :: Integer -> BitCount -> BitCount -> BitCount -> Integer
sliceInt x xBitCount s e = fromIntegral sliced where
	sliced = (x .&. startMask) `bitShiftR` (xBitCount - e)
	startMask = n1Bits (xBitCount - s)
	n1Bits n  = (2 `pow` n) - 1
\end{code}

Split a raw bit array into (sign, exponent, fraction) components. These
components have not been processed (unbiased, added significant bit,
etc).

\begin{code}
splitRawIEEE754 :: [Word8] -> (Bool, Exponent, Fraction)
splitRawIEEE754 bs = (sign, exp', frac) where
	sign = (head bs .&. 0x80) == 0x80
	exp' = Exponent (fromIntegral $ bitSlice bs 1 (1 + w))
	frac = Fraction (bitSlice bs (1 + w) (bitsInWord8 bs))
	w    = exponentWidth $ bitsInWord8 bs
\end{code}

Parse values into a form suitable for {\tt encodeFloat}.

exponent fraction width-in-bits -> fraction, exponent.

\begin{code}
mergeFloat :: Exponent -> Fraction -> BitCount -> (Integer, Int)
mergeFloat 0 0 _ = (0, 0)
mergeFloat e f width
	-- Infinity / NaN (TODO)
	| e == eMax = error "Infinity/NaN not supported"
	
	| otherwise = case e of
		-- Denormalized
		0 -> (fromIntegral f, fromIntegral unbiasedE + 1 - fromIntegral fWidth)
		
		-- Normalized
		_ -> (fromIntegral f + (1 `bitShiftL` fWidth), fromIntegral unbiasedE - fromIntegral fWidth)
		
		where eWidth    = exponentWidth width
		      fWidth    = width - eWidth - 1
		      eMax      = (2 `pow` eWidth) - 1
		      unbiasedE = unbias e eWidth
\end{code}

\section{Serialising}

\subsection{Public interface}

\begin{code}
putFloat32be :: Float -> Put
putFloat32be = putFloat (ByteCount 4) encodeIntBE
\end{code}

\begin{code}
putFloat32le :: Float -> Put
putFloat32le = putFloat (ByteCount 4) encodeIntLE
\end{code}

\begin{code}
putFloat64be :: Double -> Put
putFloat64be = putFloat (ByteCount 8) encodeIntBE
\end{code}

\begin{code}
putFloat64le :: Double -> Put
putFloat64le = putFloat (ByteCount 8) encodeIntLE
\end{code}

\subsection{Implementation}

\begin{code}
putFloat :: (RealFloat a) => ByteCount -> (ByteCount -> Integer -> [Word8]) -> a -> Put
putFloat width f v = putByteString $ B.pack words' where
	words' = f width (floatToMerged width v)
\end{code}

\begin{code}
floatComponents :: (RealFloat a) => ByteCount -> a -> (Bool, Fraction, Exponent)
floatComponents width v =
	case (dFraction, dExponent, biasedE) of
		(0, 0, _) -> (sign, 0, 0)
		(_, _, 0) -> (sign, truncatedFraction + 1, 0)
		_         -> (sign, truncatedFraction, biasedE)
	where dFraction   = Fraction $ fst (decodeFloat v)
	      dExponent   = Exponent $ snd (decodeFloat v)
	      eWidth      = exponentWidth (bitCount width)
	      fWidth      = bitCount width - eWidth - 1 -- 1 for sign bit
	      biasedE     = bias (dExponent + fromIntegral fWidth) eWidth
	      absFraction = abs dFraction
	
	      -- Weird check is for detecting -0.0
	      sign        = (1.0 / v) < 0.0
	
	      -- Fraction needs to be truncated, depending on the exponent
	      truncatedFraction = absFraction - (1 `bitShiftL` fWidth)
\end{code}

\begin{code}
floatToMerged :: (RealFloat a) => ByteCount -> a -> Integer
floatToMerged width v = mergeFloatBits' (floatComponents width v) where
	mergeFloatBits' (s, f, e) = mergeFloatBits fWidth eWidth s f e
	eWidth      = exponentWidth $ bitCount width
	fWidth      = bitCount width - eWidth - 1 -- 1 for sign bit
\end{code}

\begin{code}
mergeFloatBits :: BitCount -> BitCount -> Bool -> Fraction -> Exponent -> Integer
mergeFloatBits fWidth eWidth s f e = merged where
	merged = shiftedSign .|. shiftedFrac .|. shiftedExp
	sBit = if s then 1 else 0 :: Integer
	shiftedSign = sBit `bitShiftL` (fWidth + eWidth) :: Integer
	shiftedExp  = fromIntegral e `bitShiftL` fWidth :: Integer
	shiftedFrac = fromIntegral f
\end{code}

Encode an integer to a list of words, in big-endian format

\begin{code}
encodeIntBE :: ByteCount -> Integer -> [Word8]
encodeIntBE 0     _ = []
encodeIntBE width x = encoded where
	encoded = encodeIntBE (width - 1) (x `shiftR` 8) ++ [step]
	step = fromIntegral x .&. 0xFF
\end{code}

Encode an integer to a list of words, in little-endian format

\begin{code}
encodeIntLE :: ByteCount -> Integer -> [Word8]
encodeIntLE width = reverse . encodeIntBE width
\end{code}

\section{Exponents}

Calculate the proper size of the exponent field, in bits, given the
size of the full structure.

\begin{code}
exponentWidth :: BitCount -> BitCount
exponentWidth k
	| k == 16         = 5
	| k == 32         = 8
	| k `mod` 32 == 0 = ceiling (4 * logBase 2 (fromIntegral k)) - 13
	| otherwise       = error "Invalid length of floating-point value"
\end{code}

\begin{code}
bias :: Exponent -> BitCount -> Exponent
bias e eWidth = e - (1 - (2 `pow` (eWidth - 1)))
\end{code}

\begin{code}
unbias :: Exponent -> BitCount -> Exponent
unbias e eWidth = e + 1 - (2 `pow` (eWidth - 1))
\end{code}

\section{Byte and bit counting}

\begin{code}
newtype Exponent = Exponent Int
	deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)

newtype Fraction = Fraction Integer
	deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)

newtype BitCount = BitCount Int
	deriving (Show, Eq, Num, Ord, Real, Enum, Integral)

newtype ByteCount = ByteCount Int
	deriving (Show, Eq, Num, Ord, Real, Enum, Integral)

bitCount :: ByteCount -> BitCount
bitCount (ByteCount x) = BitCount (x * 8)

bitsInWord8 :: [Word8] -> BitCount
bitsInWord8 = bitCount . ByteCount . length

bitShiftL :: (Bits a) => a -> BitCount -> a
bitShiftL x (BitCount n) = shiftL x n

bitShiftR :: (Bits a) => a -> BitCount -> a
bitShiftR x (BitCount n) = shiftR x n
\end{code}

\section{Utility}

Integral version of {\tt (**)}

\begin{code}
pow :: (Integral a, Integral b, Integral c) => a -> b -> c
pow b e = floor $ fromIntegral b ** fromIntegral e
\end{code}

