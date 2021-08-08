module Codec.Compression.Lzo.Block ( compress1
                                   , compress9
                                   , decompress
                                   , LzoError
                                   , lzoOk
                                   , lzoError
                                   , lzoOutOfMemory
                                   , lzoNotCompressible
                                   , lzoInputOverrun
                                   , lzoOutputOverrun
                                   , lzoLookbehindOverrun
                                   , lzoEofNotFound
                                   , lzoEInputNotConsumed
                                   , lzoENotYetImplemented
                                   , lzoEInvalidArgument
                                   , lzoEInvalidAlignment
                                   , lzoEOutputNotConsumed
                                   , lzoEInternalError
                                   , lzoVersion
                                   , lzoVersionString
                                   , lzoVersionDate
                                   ) where

import Control.Exception (Exception, throw)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CChar, CInt (..), CUInt (..))
import Control.Monad (when)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

#include <lzo/lzo1x.h>

type Byte = CChar

--                 lzo_bytep   lzo_uint lzo_bytep   lzo_uintp    lzo_voidp
type LzoCompress = Ptr Byte -> CUInt -> Ptr Byte -> Ptr CUInt -> Ptr () -> IO CInt

foreign import ccall unsafe lzo1x_1_compress   :: LzoCompress
foreign import ccall unsafe lzo1x_999_compress :: LzoCompress
foreign import ccall unsafe lzo1x_decompress   :: LzoCompress

foreign import ccall unsafe lzo_version :: CUInt
foreign import ccall unsafe lzo_version_string :: CString
foreign import ccall unsafe lzo_version_date :: CString

-- | @since 0.1.1.0
lzoVersion :: Word
lzoVersion = fromIntegral lzo_version

-- | @since 0.1.1.0
lzoVersionString :: String
lzoVersionString = unsafeDupablePerformIO $ peekCString lzo_version_string

-- | @since 0.1.1.0
lzoVersionDate :: String
lzoVersionDate = unsafeDupablePerformIO $ peekCString lzo_version_date

lzo1MemCompress :: Integral a => a
lzo1MemCompress = #{const LZO1X_1_MEM_COMPRESS}

lzo999MemCompress :: Integral a => a
lzo999MemCompress = #{const LZO1X_999_MEM_COMPRESS}

newtype LzoError = LzoError CInt deriving (Eq)

instance Exception LzoError

instance Show LzoError where
    show err | err == lzoOk = "LZO_E_OK"
             | err == lzoError = "LZO_E_ERROR"
             | err == lzoOutOfMemory = "LZO_E_OUT_OF_MEMORY"
             | err == lzoNotCompressible = "LZO_E_NOT_COMPRESSIBLE"
             | err == lzoInputOverrun = "LZO_E_INPUT_OVERRUN"
             | err == lzoOutputOverrun = "LZO_E_OUTPUT_OVERRUN"
             | err == lzoLookbehindOverrun = "LZO_E_LOOKBEHIND_OVERRUN"
             | err == lzoEofNotFound = "LZO_E_EOF_NOT_FOUND"
             | err == lzoEInputNotConsumed = "LZO_E_INPUT_NOT_CONSUMED"
             | err == lzoENotYetImplemented = "LZO_E_NOT_YET_IMPLEMENTED"
             | err == lzoEInvalidArgument = "LZO_E_INVALID_ARGUMENT"
             | err == lzoEInvalidAlignment = "LZO_E_INVALID_ALIGNMENT"
             | err == lzoEOutputNotConsumed = "LZO_E_OUTPUT_NOT_CONSUMED"
             | err == lzoEInternalError = "LZO_E_INTERNAL_ERROR"
             | otherwise = "Invalid error code"

isError :: LzoError -> Bool
isError err | err /= lzoOk = True
            | otherwise = False

#{enum LzoError, LzoError
    , lzoOk = LZO_E_OK
    , lzoError = LZO_E_ERROR
    , lzoOutOfMemory = LZO_E_OUT_OF_MEMORY
    , lzoNotCompressible = LZO_E_NOT_COMPRESSIBLE
    , lzoInputOverrun = LZO_E_INPUT_OVERRUN
    , lzoOutputOverrun = LZO_E_OUTPUT_OVERRUN
    , lzoLookbehindOverrun = LZO_E_LOOKBEHIND_OVERRUN
    , lzoEofNotFound = LZO_E_EOF_NOT_FOUND
    , lzoEInputNotConsumed = LZO_E_INPUT_NOT_CONSUMED
    , lzoENotYetImplemented = LZO_E_NOT_YET_IMPLEMENTED
    , lzoEInvalidArgument = LZO_E_INVALID_ARGUMENT
    , lzoEInvalidAlignment = LZO_E_INVALID_ALIGNMENT
    , lzoEOutputNotConsumed = LZO_E_OUTPUT_NOT_CONSUMED
    , lzoEInternalError = LZO_E_INTERNAL_ERROR
  }

compressBufSz :: Integral a => a -> a
compressBufSz l' = l' + (l' `div` 16) + 64 + 3

compress1 :: BS.ByteString -> BS.ByteString
compress1 = compress lzo1MemCompress lzo1x_1_compress

compress9 :: BS.ByteString -> BS.ByteString
compress9 = compress lzo999MemCompress lzo1x_999_compress

compress :: Int -> LzoCompress -> BS.ByteString -> BS.ByteString
compress workmem lzoCompress inBs = unsafePerformIO $
    allocaBytes workmem $ \memBuf ->
        BS.unsafeUseAsCStringLen inBs $ \(buf, bufSz) ->
            allocaBytes (compressBufSz bufSz) $ \bytePtr ->
                alloca $ \szPtr -> do
                    res <- LzoError <$> lzoCompress buf (fromIntegral bufSz) bytePtr szPtr memBuf
                    when (isError res) $
                        throw res
                    sz <- peek szPtr
                    BS.packCStringLen (bytePtr, fromIntegral sz)

decompress :: BS.ByteString
           -> Int -- ^ Maximum bound on output bytes
           -> BS.ByteString
decompress inBs outSz = unsafePerformIO $
    BS.unsafeUseAsCStringLen inBs $ \(buf, bufSz) ->
        allocaBytes outSz $ \bytePtr ->
            alloca $ \szPtr -> do
                poke szPtr (fromIntegral outSz)
                res <- LzoError <$> lzo1x_decompress buf (fromIntegral bufSz) bytePtr szPtr nullPtr
                when (isError res) $
                    throw res
                sz <- peek szPtr
                BS.packCStringLen (bytePtr, fromIntegral sz)
