
{-# LANGUAGE Strict #-}
module Leopard.Codec
  ( LeopardResult
  ) 
  where


--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Array
import Data.Semigroup
import Data.Monoid

import Data.ByteString (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.ByteString.Builder as BB

import Leopard.Binding
import Leopard.Types
import Leopard.Misc

--------------------------------------------------------------------------------

{-

{-# NOINLINE #-}
encodeIO :: ECParams -> ByteString -> IO EncodedData
encodeIO ecParams@(ECParams k n) rawInput = do

  let m = n - k

  let orig_size    = B.length input
  let chunk_size_0 = ceilDiv orig_size k
  let chunk_size   = roundUpToMultipleOf 64 chunk_size_0 

--------------------------------------------------------------------------------

partitionLazyBS :: Int -> L.ByteString -> L.ByteString

prependLengthAndPad :: Int -> ByteString -> L.ByteString
prependLengthAndPad padToMultipleOf bs = where
  len     = fromIntegral (B.length bs) :: Word64
  builder = BB.word64BE len <> BB.byteString bs <> BB.byteString padding
  padding = B.pack (replicate padlen 0)
  padlen  = requiredPadToMultipleOf (fromIntegral len + 8)

-}
