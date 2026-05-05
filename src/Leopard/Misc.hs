
{-# LANGUAGE Strict #-}
module Leopard.Misc where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import System.Random

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import Text.Printf

import Data.ByteString (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI

--------------------------------------------------------------------------------
-- * Integer logarithm

-- | Largest integer @k@ such that @2^k@ is smaller or equal to @n@
integerLog2' :: Integer -> Int
integerLog2' n = go n where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2' :: Integer -> Int
ceilingLog2' 0 = 0
ceilingLog2' n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

integerLog2 :: Int -> Int
integerLog2 = integerLog2' . fromIntegral

ceilingLog2 :: Int -> Int
ceilingLog2 = ceilingLog2' . fromIntegral
  
--------------------------------------------------------------------------------
-- * Division

-- | @ceil( a / b )@
ceilDiv :: Int -> Int -> Int
ceilDiv a b = div (a+b-1) b

isDivisibleBy64 :: Int -> Bool
isDivisibleBy64 n = (mod n 64 == 0)

-- | Rounding up to the multiple of the first argument
roundUpToMultipleOf :: Int -> Int -> Int
roundUpToMultipleOf size x = size * (ceilDiv x size)

requiredPadToMultipleOf :: Int -> Int -> Int
requiredPadToMultipleOf size x = roundUpToMultipleOf size x - x

--------------------------------------------------------------------------------
-- * Bytestrings

partitionBS :: Int -> ByteString -> [ByteString]
partitionBS len = go where
  go :: ByteString -> [ByteString]
  go bs = if B.null bs
    then []
    else B.take len bs : go (B.drop len bs)

withByteString :: ByteString -> (Int -> Ptr Word8 -> IO a) -> IO a
withByteString bs@(BI.BS fptr len) action = 
  withForeignPtr fptr $ \ptr -> action len ptr

createByteString :: Int -> Ptr Word8 -> IO ByteString
createByteString len src = BI.create len $ \tgt -> copyBytes tgt src len

randomByteString :: Int -> IO ByteString
randomByteString len = do
  xs <- replicateM len randomIO :: IO [Word8]
  return (B.pack xs)

byteStringToHexString :: ByteString -> String
byteStringToHexString = concatMap f . B.unpack where
  f :: Word8 -> String
  f = printf "%02x"

--------------------------------------------------------------------------------
-- * Arrays

arrayLength :: Array Int a -> Int
arrayLength arr = let (u,v) = bounds arr in v - u + 1 

arrayFromList :: [a] -> Array Int a
arrayFromList xs = listArray (0,length xs - 1) xs

--------------------------------------------------------------------------------
-- * Random masks

-- | There will be @k@ @Nothing@-s in the resulting array
maskRandomly :: Int -> Array Int a -> IO (Array Int (Maybe a))
maskRandomly k arr = do
  mask <- randomBoolMask (arrayLength arr) k
  let (u,v) = bounds arr
  return $ listArray (u,v) 
    [ if b then Just x else Nothing | (x,b) <- zip (elems arr) (elems mask) ]

-- | @randomBoolMask n k@ will give you @k@ falses and @(n-k)@ trues
randomBoolMask :: Int -> Int -> IO (Array Int Bool)
randomBoolMask n k = go k trues where

  trues :: Array Int Bool
  trues = listArray (0,n-1) (replicate n True)

  go :: Int -> Array Int Bool -> IO (Array Int Bool)
  go 0 arr = return arr
  go k arr = do
    j <- randomRIO (0,n-1)
    case arr!j of 
      True  -> go (k-1) (arr // [(j,False)])
      False -> go  k     arr

--------------------------------------------------------------------------------
-- * Marshal

allocaArrays :: Storable a => [Int] -> ([Ptr a] -> IO b) -> IO b
allocaArrays sizes action = go sizes [] where
  go []     ptrs = action (reverse ptrs)
  go (k:ks) ptrs = allocaArray k $ \ptr -> go ks (ptr : ptrs)

--------------------------------------------------------------------------------
-- * Monad

flipZipWithM_ :: Monad m => [a] -> [b] -> (a -> b -> m ()) -> m ()
flipZipWithM_ xs ys action = zipWithM_ action xs ys

--------------------------------------------------------------------------------
-- * Misc

-- | If all the elements of the input list are the same, then it returns that element
isUniformList :: Eq a => [a] -> Maybe a
isUniformList [] = error "isUniformList: empty input"
isUniformList (x0:x0s) = go x0s where
  go []     = Just x0
  go (u:us) = if u == x0 
    then go us
    else Nothing

isUniformList_ :: Eq a => [a] -> a
isUniformList_ xs = case isUniformList xs of
  Just x  -> x
  Nothing -> error "isUniformList_: not an uniform list"

--------------------------------------------------------------------------------

