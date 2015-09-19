module SCSI.MMC.ReadTocPmaAtip where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Parser
import SCSI

data AddressFormat = LBA | MSF
    deriving (Eq, Show)

-- * Response Format 0000b: Formatted TOC

data FormattedToc = FormattedToc {
    length_          :: Word16,
    firstTrackNumber :: Word8,
    lastTrackNumber  :: Word8,
    trackDescriptors :: [TrackDescriptor]
} deriving Show

data TrackDescriptor = TrackDescriptor {
    adr               :: Word8,
    control           :: Word8,
    trackNumber       :: Word8,
    trackStartAddress :: Word32
} deriving Show

parseFormattedToc :: Parser FormattedToc
parseFormattedToc = do
    length_          <- word16BE
    firstTrackNumber <- word8
    lastTrackNumber  <- word8
    trackDescriptors <- replicateM ((fromIntegral length_ - 2) `div` 8) parseTrackDescriptor
    return $ FormattedToc length_ firstTrackNumber lastTrackNumber trackDescriptors
    
parseTrackDescriptor :: Parser TrackDescriptor
parseTrackDescriptor = do
    word8
    [adr,control]     <- bits8 [4,4]
    trackNumber       <- word8
    word8
    trackStartAddress <- word32BE
    return $ TrackDescriptor adr control trackNumber trackStartAddress

readFormattedToc :: ScsiDevice -> AddressFormat -> Word8 -> IO FormattedToc
readFormattedToc device addressFormat start = do
    let addressFormat' = case addressFormat of { LBA -> 0x00; MSF -> 0x02 }
    Left res <- commandR device 65536
                    [0x43,addressFormat',0x00,0x00,0x00,0x00,start,0xFF,0xFF,0x00]
    return (runParser parseFormattedToc res)
