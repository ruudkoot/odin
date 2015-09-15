{-# LANGUAGE NamedFieldPuns #-}

module SCSI where

import Control.Applicative ((<$>), (<*>))
import Foreign

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
    dxferp          :: Ptr (),
    cmdp            :: Ptr (),
    sbp             :: Ptr (),
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
