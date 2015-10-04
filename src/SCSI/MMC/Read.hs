module SCSI.MMC.Read (
    read10
) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString
import Data.Word
import Parser
import SCSI

read10 :: ScsiDevice -> Word32 -> IO (Either ByteString ScsiError)
read10 device lba = do
    commandR device 2048 $
        [0x28,0x00] ++ bigEndian lba ++ [0x00,0x00,0x01,0x00]
