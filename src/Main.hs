module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Environment
import System.Posix

import SCSI

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

-- | IO control

c_ioctl' :: Storable d => Fd -> Int -> Ptr d -> IO ()
c_ioctl' fd req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fromIntegral fd) (fromIntegral req) (castPtr p)

ioctl :: Storable d => Fd -> Int -> d -> IO d
ioctl fd req d = with d $ \p -> c_ioctl' fd req p >> peek p

-- | Main

main :: IO ()
main = do
    [filePath] <- getArgs
    fd <- openFd filePath ReadOnly Nothing defaultFileFlags
    if fd > 0 then
        doScsiStuff fd
    else
        print "could not open file"
    closeFd fd
    return ()

bytesToString :: Ptr a -> Int -> Int -> IO String
bytesToString buffer start len
    = forM [start .. start + len - 1] $ \i -> do
        byte <- peekByteOff buffer i :: IO Word8
        let char = chr (fromIntegral byte)
        return char

doScsiStuff :: Fd -> IO ()
doScsiStuff fd = do

    let sense_len = 32
    let block_len = 512
    let cdb_len   = 6

    sense_buffer <- mallocBytes        sense_len
    data_buffer  <- mallocBytes (256 * block_len)
    cdb          <- mallocBytes          cdb_len
    
    pokeByteOff cdb 0 (0x12 :: Word8)
    pokeByteOff cdb 1 (0x00 :: Word8)
    pokeByteOff cdb 2 (0x00 :: Word8)
    pokeByteOff cdb 3 (0x00 :: Word8)
    pokeByteOff cdb 4 (0xff :: Word8)
    pokeByteOff cdb 5 (0x00 :: Word8)

    let inquiry = SG_IO_HDR {
            interface_id    = fromIntegral $ ord 'S',
            dxfer_direction = sg_dxfer_from_dev,
            cmd_len         = fromIntegral $ cdb_len,
            mx_sb_len       = fromIntegral $ sense_len,
            iovec_count     = 0,
            dxfer_len       = fromIntegral $ block_len,
            dxferp          = data_buffer,
            cmdp            = cdb,
            sbp             = sense_buffer,
            timeout         = 0,
            flags           = sg_flag_unused_lun_inhibit,
            pack_id         = nullPtr,
            usr_ptr         = nullPtr,
            status          = 0,
            masked_status   = 0,
            msg_status      = 0,
            sb_len_wr       = 0,
            host_status     = 0,
            driver_status   = 0,
            resid           = 0,
            duration        = 0,
            info            = 0
        }

    res <- ioctl fd sg_io inquiry
    
    if status res /= 0 then
        print "FAILED!"
    else do
        vendor  <- bytesToString data_buffer  8  8
        product <- bytesToString data_buffer 16 16
        version <- bytesToString data_buffer 32  4
            
        putStrLn $ "Vendor : " ++ vendor
        putStrLn $ "Product: " ++ product
        putStrLn $ "Version: " ++ version

    free cdb
    free data_buffer
    free sense_buffer
        
    return ()
