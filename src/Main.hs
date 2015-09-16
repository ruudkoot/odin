module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString as BS
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

bytesToString :: BS.ByteString -> Int -> Int -> String
bytesToString buffer start len
    = map (chr . fromIntegral) $ BS.unpack $ BS.take len $ BS.drop start buffer

doScsiStuff :: Fd -> IO ()
doScsiStuff fd = do

    let sense_len = 32
    let block_len = 512
    let cdb_len   = 6
    
    let sense_buffer = BS.replicate        sense_len  0x00
    let data_buffer  = BS.replicate (256 * block_len) 0x00
    let cdb          = BS.pack [0x12, 0x00, 0x00, 0x00, 0xff, 0x00]

    res <- BS.useAsCStringLen sense_buffer $ \(sense_buffer', _) ->
           BS.useAsCStringLen data_buffer $ \(data_buffer', _) ->
           BS.useAsCStringLen cdb $ \(cdb', _) -> do

            let inquiry = SG_IO_HDR {
                    interface_id    = fromIntegral $ ord 'S',
                    dxfer_direction = sg_dxfer_from_dev,
                    cmd_len         = fromIntegral $ cdb_len,
                    mx_sb_len       = fromIntegral $ sense_len,
                    iovec_count     = 0,
                    dxfer_len       = fromIntegral $ block_len,
                    dxferp          = data_buffer',
                    cmdp            = cdb',
                    sbp             = sense_buffer',
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
                return Nothing
            else do
                bs <- BS.packCStringLen (dxferp res, fromIntegral $ dxfer_len res)
                return (Just bs)
    
    case res of
        Nothing -> print "FAILED!"
        Just data_buffer -> do
            let vendor  = bytesToString data_buffer  8  8
            let product = bytesToString data_buffer 16 16
            let version = bytesToString data_buffer 32  4
                
            putStrLn $ "Vendor : " ++ vendor
            putStrLn $ "Product: " ++ product
            putStrLn $ "Version: " ++ version

    return ()
