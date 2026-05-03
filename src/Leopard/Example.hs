
module Leopard.Example where

--------------------------------------------------------------------------------

import Data.Word
import Data.Array
import Data.Maybe

import Control.Monad
import System.Random

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Leopard.Codec
import Leopard.Binding
import Leopard.Types
import Leopard.Misc

--------------------------------------------------------------------------------

init_ :: IO ()
init_ = initLeopard 

--------------------------------------------------------------------------------

maxChunks :: Int
maxChunks = 20

exampleLowLevel :: IO ()
exampleLowLevel = void (exampleLowLevel' True)

testLowLevel :: Int -> IO Bool
testLowLevel howMany = do
  oks <- replicateM howMany (exampleLowLevel' False)
  return (and oks)

exampleLowLevel' :: Bool -> IO Bool
exampleLowLevel' doPrint = withLeopard $ do

  k <- randomRIO (2,maxChunks)
  m <- randomRIO (1,k)
  let n = k + m
  let ecp = ECParams
        { _ecK = k
        , _ecN = n
        }

  -- let chunkSize = 64
  chunkSize <- ((\x -> x * 64) <$> randomRIO (1,100))

  exampleLowLevel'' ecp chunkSize doPrint

--------------------------------------------------------------------------------

exampleLowLevel'' :: ECParams -> Int -> Bool -> IO Bool
exampleLowLevel'' ecp@(ECParams k n) chunkSize doPrint = do

  let m = n - k

  when doPrint $ do
    putStrLn "Leopard example (low level)"
    putStrLn "---------------------------"
    putStrLn $ "K = " ++ show k
    putStrLn $ "N = " ++ show n
    putStrLn $ "M = " ++ show m
    putStrLn $ "chunk size = " ++ show chunkSize ++ " bytes"

  origs  <- replicateM k (randomByteString chunkSize)
  parity <- failIfLeft =<< unsafeEncodeIOList ecp origs

  let encoded = arrayFromList (origs ++ parity)
  nbad <- randomRIO (0,m)
  when doPrint $ putStrLn $ "#lost chunks = " ++ show nbad

  partial <- elems <$> maskRandomly nbad encoded
  let ngood = sum [ 1 | Just _ <- partial ]
  unless (nbad + ngood == n) $ error "fatal: nbad + ngood /= N"

  -- when doPrint $ print $ map isJust partial

  decoded <- failIfLeft =<< unsafeDecodeIOList ecp partial

  let ok = (origs == decoded)
  when doPrint $ putStrLn $ "reconstruction successful = " ++ show ok

{-
  when doPrint $ do
    printChunks "original"      origs
    printChunks "parity"        parity
    printChunks "reconstructed" decoded
-}

  return ok

--------------------------------------------------------------------------------

failIfLeft :: Either LeopardResult a -> IO a
failIfLeft (Left  err) = fail (show $ decodeLeopardResult err)
failIfLeft (Right res) = return res

--------------------------------------------------------------------------------

printChunks :: String -> [ByteString] -> IO ()
printChunks title bss = do
  putStrLn ""
  putStrLn title
  putStrLn (replicate (length title) '-')
  flipZipWithM_ [0..] bss $ \idx bs -> do
    putStrLn $ " - " ++ show idx ++ ": " ++ byteStringToHexString bs

--------------------------------------------------------------------------------
