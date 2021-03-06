{-# LANGUAGE NamedFieldPuns #-}

module SCSI (
    ScsiDevice,
    ScsiError(..),
    withScsiDevice,
    scsiCommand,
    commandN,
    commandR,
    commandW,
    commandB
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import Data.Char
import Foreign
import Foreign.C
import System.Posix

import Parser
import SCSI.Sense

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

-- | IO control

c_ioctl' :: Storable d => Fd -> Int -> Ptr d -> IO ()
c_ioctl' fd req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fromIntegral fd) (fromIntegral req) (castPtr p)

ioctl :: Storable d => Fd -> Int -> d -> IO d
ioctl fd req d = with d $ \p -> c_ioctl' fd req p >> peek p


-- | Linux SCSI Generic

sg_dxfer_none, sg_dxfer_to_dev, sg_dxfer_from_dev, sg_dxfer_to_from_dev :: Int32
sg_dxfer_none           = (-1)
sg_dxfer_to_dev         = (-2)
sg_dxfer_from_dev       = (-3)
sg_dxfer_to_from_dev    = (-4)
-- sg_dxfer_unknown        = (-5)

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

-- FIXME: currently 64-bit only!
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
    = ScsiErrorSense SenseData
  deriving Show
    
withScsiDevice :: FilePath -> (ScsiDevice -> IO ()) -> IO ()
withScsiDevice filePath f = do
    fd <- openFd filePath ReadOnly Nothing defaultFileFlags
    if fd > 0 then
        f fd
    else do
        putStrLn $ "withScsiDevice: could not open '" ++ filePath ++ "'"
    closeFd fd
    
scsiCommand :: Fd -> Int32 -> BS.ByteString -> [Word8] -> IO (Either BS.ByteString ScsiError)
scsiCommand fd dir data_buffer cdb' = do

    let sense_len    = 252
    let sense_buffer = BS.replicate sense_len 0x00
    let cdb          =
            if length cdb' `elem` [6, 10, 12] then
                BS.pack cdb'
            else
                error "scsiCommand: CDB should be 6, 10 or 12 bytes long"

    res <- BS.useAsCStringLen sense_buffer $ \(sense_buffer', _) ->
            BS.useAsCStringLen data_buffer $ \(data_buffer', data_len) ->
             BS.useAsCStringLen cdb $ \(cdb', cdb_len) -> do

                let inquiry = SG_IO_HDR {
                        interface_id    = fromIntegral $ ord 'S',
                        dxfer_direction = dir,
                        cmd_len         = fromIntegral $ cdb_len,
                        mx_sb_len       = fromIntegral $ sense_len,
                        iovec_count     = 0,
                        dxfer_len       = fromIntegral $ data_len,
                        dxferp          = if dir == sg_dxfer_none then
                                            nullPtr
                                          else
                                            data_buffer',
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
                
                case masked_status res of
                    0 -> do
                        bs <- BS.packCStringLen (dxferp res, fromIntegral $ dxfer_len res)
                        return (Left bs)
                    1 -> do
                        sd <- runParser parseSenseData <$> BS.packCStringLen
                                (sbp res, fromIntegral $ sb_len_wr res)
                        return (Right (ScsiErrorSense sd))

    return res
    
commandN :: ScsiDevice -> [Word8] -> IO (Either () ScsiError)
commandN device cdb
    = do let dataIn = BS.empty
         res <- scsiCommand device sg_dxfer_none dataIn cdb 
         return $ case res of
            Left  _   -> Left ()
            Right err -> Right err

commandR :: ScsiDevice -> Int -> [Word8] -> IO (Either BS.ByteString ScsiError)
commandR device n cdb
    = let dataIn = BS.replicate n 0x00
       in scsiCommand device sg_dxfer_from_dev dataIn cdb

commandW :: ScsiDevice -> BS.ByteString -> [Word8] -> IO (Either () ScsiError)
commandW device dataIn cdb
    = do res <- scsiCommand device sg_dxfer_to_dev dataIn cdb
         return $ case res of
            Left  _   -> Left ()
            Right err -> Right err

commandB :: ScsiDevice -> BS.ByteString -> [Word8] -> IO (Either BS.ByteString ScsiError)
commandB device = scsiCommand device sg_dxfer_to_from_dev
