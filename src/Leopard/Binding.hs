
-- | Note: This is an internal module; use @Leopard.Codec@ instead

{-# LANGUAGE ForeignFunctionInterface, CPP, Strict, ScopedTypeVariables #-}
module Leopard.Binding where

--------------------------------------------------------------------------------

import Data.Word
import Data.Array
import Data.Maybe

import Control.Monad

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Leopard.Types
import Leopard.Misc

--------------------------------------------------------------------------------
-- * error handling

data LeopardResult
  = Success                           -- ^ Operation succeeded
  | NeedMoreData                      -- ^ Not enough recovery data received
  | TooMuchData                       -- ^ Buffer counts are too high
  | InvalidSize                       -- ^ Buffer size must be a multiple of 64 bytes
  | InvalidCounts                     -- ^ Invalid counts provided
  | InvalidInput                      -- ^ A function parameter was invalid
  | Platform                          -- ^ Platform is unsupported
  | CallInitialize                    -- ^ Call leo_init() first
  deriving (Eq,Show)

instance Enum LeopardResult where

  toEnum ( 0)  = Success             -- Operation succeeded
  toEnum (-1)  = NeedMoreData        -- Not enough recovery data received
  toEnum (-2)  = TooMuchData         -- Buffer counts are too high
  toEnum (-3)  = InvalidSize         -- Buffer size must be a multiple of 64 bytes
  toEnum (-4)  = InvalidCounts       -- Invalid counts provided
  toEnum (-5)  = InvalidInput        -- A function parameter was invalid
  toEnum (-6)  = Platform            -- Platform is unsupported
  toEnum (-7)  = CallInitialize      -- Call leo_init() first 

  toEnum _     = error "invalid leopard error code"

  fromEnum _ = error "LeopardResult/fromEnum: not implemented"

decodeLeopardResult :: LeopardResult -> Maybe String
decodeLeopardResult result = case result of
  Success        -> Nothing  -- "Operation succeeded"
  NeedMoreData   -> Just "Not enough recovery data received"
  TooMuchData    -> Just "Buffer counts are too high"
  InvalidSize    -> Just "Buffer size must be a multiple of 64 bytes"
  InvalidCounts  -> Just "Invalid counts provided"
  InvalidInput   -> Just "A function parameter was invalid"
  Platform       -> Just "Platform is unsupported"
  CallInitialize -> Just "Call leo_init() first"

--------------------------------------------------------------------------------
-- * C++ bindings

{-# NOINLINE initLeopard #-}
initLeopard :: IO ()
initLeopard = do
  res <- cpp_leo_init leo_VERSION
  if (res == 0)
    then return ()
    else fail "Leopard initialization failed"

withLeopard :: IO a -> IO a
withLeopard action = do
  initLeopard
  action

unsafeEncodeIOList :: ECParams -> [ByteString] -> IO (Either LeopardResult [ByteString])
unsafeEncodeIOList ecParams inputChunks = do
  ei <- unsafeEncodeIO ecParams (arrayFromList inputChunks)
  return $ case ei of
    Left  err -> Left err
    Right arr -> Right (elems arr) 

--------------------------------------------------------------------------------

-- | Takes @K@ input chunks, and returns @M@ parity chunks.
--
-- We assume that the chunks have a size which is a multiple of 64 bytes, as 
-- the underlying `leopard` library assumes that too...
--
{-# NOINLINE unsafeEncodeIO #-}
unsafeEncodeIO :: ECParams -> Array Int ByteString -> IO (Either LeopardResult (Array Int ByteString))
unsafeEncodeIO ecParams@(ECParams k n) inputChunks = do
  let m = n - k
  work_cnt <- cpp_leo_encode_work_count (fromIntegral k) (fromIntegral m)
  when (work_cnt == 0) $ fail "encode: `leo_encode_work_count` claims invalid input"
  let work_cnt_int = fromIntegral work_cnt :: Int

  let nchunks       = arrayLength inputChunks
  let sizes         = map B.length (elems inputChunks)
  let mb_chunk_size = isUniformList sizes

  unless (k == nchunks)        $ fail "encode: we need exactly K input chunks"  
  unless (isJust mb_chunk_size) $ fail "encode: chunk size must be uniform"

  let chunk_size = fromJust mb_chunk_size
  unless (isDivisibleBy64 chunk_size) $ fail "encode: chunk size should be divisible by 64"

  allocaArray nchunks $ \(porigs :: Ptr PtrWord8) -> do
    flipZipWithM_ [0..] (elems inputChunks) $ \idx bs -> withByteString bs $ \len ptr -> pokeElemOff porigs idx ptr

    allocaArrays (replicate work_cnt_int chunk_size) $ \(ptrs :: [PtrWord8]) -> do 
      allocaArray work_cnt_int $ \(pworks :: Ptr PtrWord8) -> do
        flipZipWithM_ [0..] ptrs $ \idx ptr -> pokeElemOff pworks idx ptr
          
        res <- cpp_leo_encode 
          (fromIntegral chunk_size)     -- Number of bytes in each data buffer                                                    
          (fromIntegral k)              -- Number of original_data[] buffer pointers                                     
          (fromIntegral m)              -- Number of recovery_data[] buffer pointers                                     
          (fromIntegral work_cnt)       -- Number of work_data[] buffer pointers, from leo_encode_work_count()                  
          porigs                        -- Array of pointers to original data buffers                          
          pworks                        -- Array of work buffers                                               

        if res /= 0 
          then return (Left $ toEnum $ fromIntegral res)
          else do
            parityChunks <- forM [0..m-1] $ \j -> do
              ptr <- peekElemOff pworks j
              createByteString chunk_size ptr
             
            return $ Right $ listArray (0,m-1) parityChunks

--------------------------------------------------------------------------------

unsafeDecodeIOList :: ECParams -> [Maybe ByteString] -> IO (Either LeopardResult [ByteString])
unsafeDecodeIOList ecParams mbChunks = do
  ei <- unsafeDecodeIO ecParams (arrayFromList mbChunks)
  return $ case ei of
    Left  err -> Left err
    Right arr -> Right (elems arr) 

{-# NOINLINE unsafeDecodeIO #-}
unsafeDecodeIO :: ECParams -> Array Int (Maybe ByteString) -> IO (Either LeopardResult (Array Int ByteString))
unsafeDecodeIO ecParams@(ECParams k n) mbChunks = do
  let m = n - k
  work_cnt <- cpp_leo_decode_work_count (fromIntegral k) (fromIntegral m)
  when (work_cnt == 0) $ fail "edeode: `leo_decode_work_count` claims invalid input"
  let work_cnt_int = fromIntegral work_cnt :: Int

  let nchunks       = arrayLength mbChunks
  let sizes         = map B.length (catMaybes $ elems mbChunks)
  let mb_chunk_size = isUniformList sizes

  unless (n == nchunks)         $ fail "encode: we need exactly N encoded chunks"    
  unless (isJust mb_chunk_size) $ fail "decode: chunk size must be uniform"

  let chunk_size = fromJust mb_chunk_size
  unless (isDivisibleBy64 chunk_size) $ fail "decode: chunk size should be divisible by 64"

  let (origChunks,parityChunks) = splitAt k (elems mbChunks)

  allocaArray k $ \(porigs :: Ptr PtrWord8) -> do
    flipZipWithM_ [0..] origChunks $ \idx mb -> case mb of
      Just bs  ->  withByteString bs $ \len ptr -> pokeElemOff porigs idx ptr
      Nothing  ->  pokeElemOff porigs idx nullPtr

    allocaArray k $ \(pparity :: Ptr PtrWord8) -> do
      flipZipWithM_ [0..] parityChunks $ \idx mb -> case mb of
        Just bs  ->  withByteString bs $ \len ptr -> pokeElemOff pparity idx ptr
        Nothing  ->  pokeElemOff pparity idx nullPtr

      allocaArrays (replicate work_cnt_int chunk_size) $ \(ptrs :: [PtrWord8]) -> do 
        allocaArray work_cnt_int $ \(pworks :: Ptr PtrWord8) -> do
          flipZipWithM_ [0..] ptrs $ \idx ptr -> pokeElemOff pworks idx ptr
            
          res <- cpp_leo_decode 
            (fromIntegral chunk_size)     -- Number of bytes in each data buffer                                                    
            (fromIntegral k)              -- Number of original_data[] buffer pointers                                     
            (fromIntegral m)              -- Number of recovery_data[] buffer pointers                                     
            (fromIntegral work_cnt)       -- Number of work_data[] buffer pointers, from leo_encode_work_count()                  
            porigs                        -- Array of pointers to original data buffers                          
            pparity                       -- Array of recovery data buffers
            pworks                        -- Array of work buffers                                               
  
          if res /= 0 
            then return (Left $ toEnum $ fromIntegral res)
            else do
              finalChunks <- forM [0..k-1] $ \j -> case origChunks!!j of
                Just orig -> return orig
                Nothing   -> do
                  ptr <- peekElemOff pworks j
                  createByteString chunk_size ptr
               
              return $ Right $ listArray (0,k-1) finalChunks

--------------------------------------------------------------------------------

type PtrWord8 = Ptr Word8

leo_VERSION :: CInt
leo_VERSION = 2

foreign import ccall "leo_init_"  cpp_leo_init :: CInt -> IO CInt

foreign import ccall "leo_result_string" cpp_leo_result_string :: CInt -> IO CString

----------------------------------------

{-
    LEO_EXPORT unsigned leo_encode_work_count(
        unsigned original_count,
        unsigned recovery_count);
-}

foreign import ccall "leo_encode_work_count" cpp_leo_encode_work_count :: CUInt -> CUInt -> IO CUInt

foreign import ccall "leo_decode_work_count" cpp_leo_decode_work_count :: CUInt -> CUInt -> IO CUInt

----------------------------------------

{-
    LEO_EXPORT LeopardResult leo_encode(
        uint64_t buffer_bytes,                    // Number of bytes in each data buffer
        unsigned original_count,                  // Number of original_data[] buffer pointers
        unsigned recovery_count,                  // Number of recovery_data[] buffer pointers
        unsigned work_count,                      // Number of work_data[] buffer pointers, from leo_encode_work_count()
        const void* const * const original_data,  // Array of pointers to original data buffers
        void** work_data);                        // Array of work buffers
-}

--
-- * `buffer_bytes` must be a multiple of 64
-- * Each buffer should have the same number of bytes.
-- * Even the last piece must be rounded up to the block size.
-- * The first set of recovery_count buffers in work_data will be the result.
--
foreign import ccall "leo_encode" cpp_leo_encode :: Word64 -> CUInt -> CUInt -> CUInt -> Ptr (Ptr a) -> Ptr (Ptr a) -> IO CInt

{-
    LEO_EXPORT LeopardResult leo_decode(
        uint64_t buffer_bytes,                    // Number of bytes in each data buffer
        unsigned original_count,                  // Number of original_data[] buffer pointers
        unsigned recovery_count,                  // Number of recovery_data[] buffer pointers
        unsigned work_count,                      // Number of buffer pointers in work_data[]
        const void* const * const original_data,  // Array of original data buffers
        const void* const * const recovery_data,  // Array of recovery data buffers
        void** work_data);        
-}

foreign import ccall "leo_decode" cpp_leo_decode :: Word64 -> CUInt -> CUInt -> CUInt -> Ptr (Ptr a) -> Ptr (Ptr a) -> Ptr (Ptr a) -> IO CInt

--------------------------------------------------------------------------------
