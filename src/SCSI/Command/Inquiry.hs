module SCSI.Command.Inquiry (
    Inquiry(..),
    DeviceType(..),
    inquiry
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Parser
import SCSI

data Inquiry = Inquiry {
    deviceType  :: DeviceType,
    vendor      :: String,
    product     :: String,
    version     :: String,
    serial      :: String
} deriving Show

data DeviceType
    = DirectAccessBlock                 -- SBC-3
    | SequentialAccessBlock             -- SSC-3
    | Printer                           -- SSC
    | Processor                         -- SPC-2
    | WriteOnce                         -- SBC
    | MultiMedia                        -- MMC-6
    | Scanner
    | OpticalMemory                     -- SBC
    | MediumChanger                     -- SMC-3
    | Communications
    | ObsoleteA
    | ObsoleteB
    | StorageArrayController            -- SCC-2
    | EnclosureServices                 -- SES
    | SimplifiedDirectAccess            -- RBC
    | OpticalCardReaderWriter           -- OCRW
    | BrideControllerCommands           -- BCC
    | ObjectBasedStorage                -- OSD
    | AutomationDriveInterface          -- ADC-2
    | Reserved13
    | Reserved14
    | Reserved15
    | Reserved16
    | Reserved17
    | Reserved18
    | Reserved19
    | Reserved1A
    | Reserved1B
    | Reserved1C
    | Reserved1D
    | WellKnownLogicalUnit
    | Unknown
  deriving (Enum, Show)

parseInquiry :: Parser Inquiry
parseInquiry = do
    [deviceType, _] <- bits8 [5,3]
    replicateM 7 word8
    vendor          <- ascii  8
    product         <- ascii 16
    version         <- ascii  4
    serial          <- ascii  8
    return $ Inquiry (toEnum (fromIntegral deviceType)) vendor product version serial
    
inquiry :: ScsiDevice -> IO Inquiry
inquiry device = do
    Left res <- commandR device 65536 [0x12, 0x00, 0x00, 0x00, 0xff, 0x00]
    return (runParser parseInquiry res)
