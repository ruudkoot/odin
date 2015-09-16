{-# LANGUAGE NamedFieldPuns #-}

module SCSI (
    ScsiDevice,
    ScsiError(..),
    withScsiDevice,
    scsiCommand
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import Data.Char
import Foreign
import Foreign.C
import System.Posix

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

-- | IO control

c_ioctl' :: Storable d => Fd -> Int -> Ptr d -> IO ()
c_ioctl' fd req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fromIntegral fd) (fromIntegral req) (castPtr p)

ioctl :: Storable d => Fd -> Int -> d -> IO d
ioctl fd req d = with d $ \p -> c_ioctl' fd req p >> peek p


-- | Linux SCSI Generic

sg_dxfer_from_dev :: Int32
sg_dxfer_from_dev = (-3)

sg_flag_unused_lun_inhibit :: Word32
sg_flag_unused_lun_inhibit = 2

sg_io :: Int
sg_io = 0x2285

data SG_IO_HDR = SG_IO_HDR {
    interface_id    :: Int32,
    dxfer_direction :: Int32,
    cmd_len         :: Word8,
    mx_sb_len       :: Word8,
    iovec_count     :: Word16,
    dxfer_len       :: Word32,
    dxferp          :: Ptr CChar,
    cmdp            :: Ptr CChar,
    sbp             :: Ptr CChar,
    timeout         :: Word32,
    flags           :: Word32,
    pack_id         :: Ptr (),
    usr_ptr         :: Ptr (),
    status          :: Word8,
    masked_status   :: Word8,
    msg_status      :: Word8,
    sb_len_wr       :: Word8,
    host_status     :: Word16,
    driver_status   :: Word16,
    resid           :: Int32,
    duration        :: Word32,
    info            :: Word32
}

instance Storable SG_IO_HDR where
    alignment _   = 8
    sizeOf    _   = 88
    peek      ptr = SG_IO_HDR
        <$> peekByteOff ptr 0x00
        <*> peekByteOff ptr 0x04
        <*> peekByteOff ptr 0x08
        <*> peekByteOff ptr 0x09
        <*> peekByteOff ptr 0x0a
        <*> peekByteOff ptr 0x0c
        <*> peekByteOff ptr 0x10
        <*> peekByteOff ptr 0x18
        <*> peekByteOff ptr 0x20
        <*> peekByteOff ptr 0x28
        <*> peekByteOff ptr 0x2c
        <*> peekByteOff ptr 0x30
        <*> peekByteOff ptr 0x38
        <*> peekByteOff ptr 0x40
        <*> peekByteOff ptr 0x41
        <*> peekByteOff ptr 0x42
        <*> peekByteOff ptr 0x43
        <*> peekByteOff ptr 0x44
        <*> peekByteOff ptr 0x46
        <*> peekByteOff ptr 0x48
        <*> peekByteOff ptr 0x4c
        <*> peekByteOff ptr 0x50
    poke ptr (SG_IO_HDR {
            interface_id,
            dxfer_direction,
            cmd_len,
            mx_sb_len,
            iovec_count,
            dxfer_len,
            dxferp,
            cmdp,
            sbp,
            timeout,
            flags,
            pack_id,
            usr_ptr,
            status,
            masked_status,
            msg_status,
            sb_len_wr,
            host_status,
            driver_status,
            resid,
            duration,
            info
        }) = do pokeByteOff ptr 0x00 interface_id
                pokeByteOff ptr 0x04 dxfer_direction
                pokeByteOff ptr 0x08 cmd_len
                pokeByteOff ptr 0x09 mx_sb_len
                pokeByteOff ptr 0x0a iovec_count
                pokeByteOff ptr 0x0c dxfer_len
                pokeByteOff ptr 0x10 dxferp
                pokeByteOff ptr 0x18 cmdp
                pokeByteOff ptr 0x20 sbp
                pokeByteOff ptr 0x28 timeout
                pokeByteOff ptr 0x2c flags
                pokeByteOff ptr 0x30 pack_id
                pokeByteOff ptr 0x38 usr_ptr
                pokeByteOff ptr 0x40 status
                pokeByteOff ptr 0x41 masked_status
                pokeByteOff ptr 0x42 msg_status
                pokeByteOff ptr 0x43 sb_len_wr
                pokeByteOff ptr 0x44 host_status
                pokeByteOff ptr 0x46 driver_status
                pokeByteOff ptr 0x48 resid
                pokeByteOff ptr 0x4c duration
                pokeByteOff ptr 0x50 info
                
-- | Friendly interface

type ScsiDevice = Fd

data ScsiError
    = ScsiErrorIoctl
    | ScsiErrorSense BS.ByteString
    deriving Show
    
withScsiDevice :: FilePath -> (ScsiDevice -> IO ()) -> IO ()
withScsiDevice filePath f = do
    fd <- openFd filePath ReadOnly Nothing defaultFileFlags
    if fd > 0 then
        f fd
    else do
        putStrLn $ "withScsiDevice: could not open '" ++ filePath ++ "'"
    closeFd fd

scsiCommand :: Fd -> BS.ByteString -> [Word8] -> IO (Either BS.ByteString ScsiError)
scsiCommand fd data_buffer cdb' = do

    let sense_len    = 32
    let sense_buffer = BS.replicate sense_len  0x00
    let cdb          = BS.pack      cdb'

    res <- BS.useAsCStringLen sense_buffer $ \(sense_buffer', _) ->
            BS.useAsCStringLen data_buffer $ \(data_buffer', data_len) ->
             BS.useAsCStringLen cdb $ \(cdb', cdb_len) -> do

                let inquiry = SG_IO_HDR {
                        interface_id    = fromIntegral $ ord 'S',
                        dxfer_direction = sg_dxfer_from_dev,
                        cmd_len         = fromIntegral $ cdb_len,
                        mx_sb_len       = fromIntegral $ sense_len,
                        iovec_count     = 0,
                        dxfer_len       = fromIntegral $ data_len,
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
                    return (Right ScsiErrorIoctl)
                else do
                    bs <- BS.packCStringLen (dxferp res, fromIntegral $ dxfer_len res)
                    return (Left bs)

    return res
