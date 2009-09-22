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
getFloat16be = getFloat (ByteCount 2) splitBytes
\end{code}

\begin{code}
getFloat16le :: Get Float
getFloat16le = getFloat (ByteCount 2) $ splitBytes . reverse
\end{code}

\begin{code}
getFloat32be :: Get Float
getFloat32be = getFloat (ByteCount 4) splitBytes
\end{code}

\begin{code}
getFloat32le :: Get Float
getFloat32le = getFloat (ByteCount 4) $ splitBytes . reverse
\end{code}

\begin{code}
getFloat64be :: Get Double
getFloat64be = getFloat (ByteCount 8) splitBytes
\end{code}

\begin{code}
getFloat64le :: Get Double
getFloat64le = getFloat (ByteCount 8) $ splitBytes . reverse
\end{code}

\subsection{Implementation}

\subsubsection{Raw float components}

Information about the raw bit patterns in the float is stored in
{\tt RawFloat}, so they don't have to be passed around to the various
format cases.

\begin{code}
data RawFloat = RawFloat
	{ rawSign             :: Sign
	, rawExponent         :: Exponent
	, rawSignificand      :: Significand
	, rawExponentWidth    :: BitCount
	, rawSignificandWidth :: BitCount
	}
	deriving (Show)
\end{code}

Split the raw byte array into (sign, exponent, significand) components.
The exponent and signifcand are drawn directly from the bits in the
original float, and have not been unbiased or otherwise modified.

\begin{code}
splitBytes :: [Word8] -> RawFloat
splitBytes bs = RawFloat sign exp' sig expWidth sigWidth where
	nBits = bitsInWord8 bs
	sign = if head bs .&. 0x80 == 0x80
		then Negative
		else Positive
	
	expStart = 1
	expWidth = exponentWidth nBits
	expEnd = expStart + expWidth
	exp' = Exponent . fromIntegral $ bitSlice bs expStart expEnd
	
	sigWidth = nBits - expEnd
	sig  = Significand $ bitSlice bs expEnd nBits
\end{code}

\subsubsection{Encodings and special values}

The next step depends on the value of the exponent $e$, size of the
exponent field in bits $w$, and value of the significand.

\begin{table}[h]
\begin{center}
\begin{tabular}{lrl}
\toprule
Exponent                & Significand & Format \\
\midrule
$0$                     & $0$           & Zero \\
$0$                     & $> 0$         & Denormalised \\
$1 \leq e \leq 2^w - 2$ & \textit{any} & Normalised \\
$2^w-1$                 & $0$           & Infinity \\
$2^w-1$                 & $> 0$         & NaN \\
\bottomrule
\end{tabular}
\end{center}
\end{table}

There's no built-in literals for Infinity or NaN, so they
are constructed using the {\tt Read} instances for {\tt Double} and
{\tt Float}.

\begin{code}
merge :: (Read a, RealFloat a) => RawFloat -> a
merge f@(RawFloat _ e sig eWidth _)
	| e == 0 = if sig == 0
		then 0.0
		else denormalised f
	| e == eMax - 1 = if sig == 0
		then read "Infinity"
		else read "NaN"
	| otherwise = normalised f
	where eMax = 2 `pow` eWidth
\end{code}

If a value is normalised, its significand has an implied {\tt 1} bit
in its most-significant bit. The significand must be adjusted by
this value before being passed to {\tt encodeField}.

\begin{code}
normalised :: RealFloat a => RawFloat -> a
normalised f = encodeFloat fraction exp' where
	Significand sig = rawSignificand f
	Exponent exp' = unbiased - sigWidth
	
	fraction = sig + (1 `bitShiftL` rawSignificandWidth f)
	
	sigWidth = fromIntegral $ rawSignificandWidth f
	unbiased = unbias (rawExponent f) (rawExponentWidth f)
\end{code}

For denormalised values, the implied {\tt 1} bit is the least-significant
bit of the exponent.

\begin{code}
denormalised :: RealFloat a => RawFloat -> a
denormalised f = encodeFloat sig exp' where
	Significand sig = rawSignificand f
	Exponent exp' = unbiased - sigWidth + 1
	
	sigWidth = fromIntegral $ rawSignificandWidth f
	unbiased = unbias (rawExponent f) (rawExponentWidth f)
\end{code}

By composing {\tt splitBytes} and {\tt merge}, the absolute value of the
float is calculated. Before being returned to the calling function, it
must be signed appropriately.

\begin{code}
getFloat :: (Read a, RealFloat a) => ByteCount
            -> ([Word8] -> RawFloat) -> Get a
getFloat (ByteCount width) parser = do
	raw <- fmap (parser . B.unpack) $ getByteString width
	let absFloat = merge raw
	return $ case rawSign raw of
		Positive ->  absFloat
		Negative -> -absFloat
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
floatComponents :: (RealFloat a) => ByteCount -> a -> (Bool, Significand, Exponent)
floatComponents width v =
	case (dFraction, dExponent, biasedE) of
		(0, 0, _) -> (sign, 0, 0)
		(_, _, 0) -> (sign, truncatedFraction + 1, 0)
		_         -> (sign, truncatedFraction, biasedE)
	where dFraction   = Significand $ fst (decodeFloat v)
	      dExponent   = Exponent . fromIntegral $ snd (decodeFloat v)
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
mergeFloatBits :: BitCount -> BitCount -> Bool -> Significand -> Exponent -> Integer
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
data Sign = Positive | Negative
	deriving (Show)

newtype Exponent = Exponent Int
	deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)

newtype Significand = Significand Integer
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

Integral version of {\tt (**)}

\begin{code}
pow :: (Integral a, Integral b, Integral c) => a -> b -> c
pow b e = floor $ fromIntegral b ** fromIntegral e
\end{code}

