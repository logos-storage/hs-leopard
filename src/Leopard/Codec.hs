
{-# LANGUAGE Strict #-}
module Leopard.Codec
  ( LeopardResult
  ,
  ) 
  where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Array

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Leopard.Binding
import Leopard.Types
import Leopard.Misc

--------------------------------------------------------------------------------

{-
{-# NOINLINE #-}
encodeIO :: ECParams -> ByteString -> IO EncodedData
encodeIO ecParams@(ECParams k n) input 

  let m = n - k

  let orig_size    = B.length input
  let chunk_size_0 = ceilDiv orig_size k
  let chunk_size   = roundUpToMultipleOf 64 chunk_size_0 
-}

--------------------------------------------------------------------------------
