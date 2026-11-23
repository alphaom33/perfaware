{-# LANGUAGE CApiFFI #-}

module Time where
import Data.Word (Word64)
import Foreign (Ptr, nullPtr, alloca, Storable (..))
import Foreign.C (CLong ())

foreign import capi unsafe "rdtsc.h rdtsc" c_rdtsc :: IO Word64
foreign import capi unsafe "sys/time.h gettimeofday" c_gettimeofday :: Ptr TimeVal -> Ptr Word64 -> IO Word64

data TimeVal = TimeVal CLong CLong

instance Storable TimeVal where
  sizeOf _ = sizeOf (undefined :: CLong) * 2
  alignment _ = sizeOf (undefined :: CLong)
  peek ptr = do
    sec <- peekByteOff ptr 0
    usec <- peekByteOff ptr (sizeOf (undefined :: CLong))
    return $ TimeVal sec usec
  poke ptr (TimeVal sec usec) = do
    pokeByteOff ptr 0 sec
    pokeByteOff ptr (sizeOf (undefined :: CLong)) usec

unpackTimeVal :: TimeVal -> Word64
unpackTimeVal (TimeVal sec usec) = fromIntegral sec * 1_000_000 + fromIntegral usec

getOsTime :: IO Word64
getOsTime = alloca $ \time -> do
    _ <- c_gettimeofday time nullPtr
    val <- peek time
    return $ unpackTimeVal val

getRdtscTime :: Word64 -> IO Word64
getRdtscTime divisor = do
    time <- c_rdtsc
    return $ time `div` divisor


