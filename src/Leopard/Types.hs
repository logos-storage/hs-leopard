
{-# LANGUAGE Strict #-}
module Leopard.Types where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Array

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Leopard.Misc

--------------------------------------------------------------------------------

-- | Note: Recause of a restriction of the underlying Leopard library, you should have
-- @K >= 2@, @N <= 2*K@ and @N <= 65536@. 
data ECParams = ECParams
  { _ecK :: Int             -- ^ @K@ is the number of original chunks
  , _ecN :: Int             -- ^ @N@ is the number of chunks after encoding
  }
  deriving (Eq,Show)

-- | Number of \"parity\" chunks
_ecM :: ECParams -> Int           
_ecM params = _ecN params - _ecK params

isValidECParams :: ECParams -> Bool
isValidECParams (ECParams k n) = and
  [ k >= 1             -- note: while Leopard only allows `k >= 2`, we can just do replication ourselves for `k = 1`.
  , k <= 32768
  , k <= n             -- note: if `k == n`, we can simply not call Leopard at all 
  , n <= 2 * k
  ]

-- | This version only accepts what Leopard should also accept 
isValidECParamsStrict :: ECParams -> Bool
isValidECParamsStrict (ECParams k n) = and
  [ k >= 2     
  , k <  n     
  , n <= 65536
  , n <= 2 * k
  ]

--------------------------------------------------------------------------------

data Encoding = Encoding
  { _ecParams     :: ECParams        -- ^ the erasure coding parameters
  , _chunkSize    :: Int             -- ^ size of an EC chunk
  , _origDataSize :: Int             -- ^ if not divisible by @K@, it can be smaller than @K x chunkSize@
  }
  deriving (Eq,Show)

isValidEncoding :: Encoding -> Bool
isValidEncoding (Encoding params@(ECParams k n) chunkSize dataSize) = and
  [ isValidECParams params
  , chunkSize == ceilDiv dataSize k
  , isDivisibleBy64 chunkSize
  ]

--------------------------------------------------------------------------------

data EncodedData = EncodedData 
  { _encoding :: Encoding
  , _chunks   :: Array Int ByteString
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------
