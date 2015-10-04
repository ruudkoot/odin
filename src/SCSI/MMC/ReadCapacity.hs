module SCSI.MMC.ReadCapacity where

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Parser
import SCSI

data Capacity = Capacity {
    logicalBlockAddress :: Word32,
    blockLength         :: Word32
} deriving Show

parseCapacity :: Parser Capacity
parseCapacity = Capacity <$> word32BE <*> word32BE

readCapacity :: ScsiDevice -> IO Capacity
readCapacity device = do
    Left res <- commandR device 8
                    [0x25,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
    return (runParser parseCapacity res)
