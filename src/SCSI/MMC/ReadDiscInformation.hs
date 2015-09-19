module SCSI.MMC.ReadDiscInformation where

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Parser
import SCSI

data DiscInformation = DiscInformation {
    diLength                            :: Word16,
    diDataType                          :: Word8,
    diErasable                          :: Bool,
    diStateOfLastSession                :: Word8,
    diDiscStatus                        :: Word8,
    diFirstTrackOnDisc                  :: Word8,
    diNumberOfSessions                  :: Word16,
    diFirstTrackNumberInLastSession     :: Word16,
    diLastTrackNumberInLastSession      :: Word16,
    diUnrestrictedUse                   :: Bool,
    diBackGroundFormatStatus            :: Word8,
    diDiscType                          :: Word8,
    diDiscIdentification                :: Maybe Word32,
    diLastSessionLeadinStartAddress     :: Word32,
    diLastPossibleLeadoutStartAddress   :: Word32,
    diDiscBarCode                       :: Maybe Word64,
    diDiscApplicationCode               :: Maybe Word8,
    diNumberOfOpcTables                 :: Word8,
    diOpcTableEntries                   :: [Word64]
} deriving Show

parseDiscInformation :: Parser DiscInformation
parseDiscInformation = cookDiscInformation
    <$> word16BE
    <*> bits8 [3,1,2,2]
    <*> word8
    <*> word8
    <*> word8
    <*> word8
    <*> bits8 [1,1,1,1,1,1,2]
    <*> word8
    <*> word8
    <*> word8
    <*> word8
    <*> word32BE
    <*> word32BE
    <*> word32BE
    <*> word64BE
    <*> word8
    <*> word8
  where cookDiscInformation
            length_
            [dataType, erasable, stateOfLastSession, discStatus]
            firstTrackOnDisc
            numberOfSessionsLSB
            firstTrackNumberInLastSessionLSB
            lastTrackNumberInLastSessionLSB
            [discIdentificationValid, discBarCodeValid, unrestrictedUse,
                discApplicationCodeValid, _, _, backGroundFormatStatus]
            discType
            numberOfSessionsMSB
            firstTrackNumberInLastSessionMSB
            lastTrackNumberInLastSessionMSB
            discIdentification
            lastSessionLeadinStartAddress
            lastPossibleLeadoutStartAddress
            discBarCode
            discApplicationCode
            numberOfOpcTables
          = DiscInformation {
                diLength                            = length_,
                diDataType                          = dataType,
                diErasable                          = bit2bool erasable,
                diStateOfLastSession                = stateOfLastSession,
                diDiscStatus                        = discStatus,
                diFirstTrackOnDisc                  = firstTrackOnDisc,
                diNumberOfSessions
                    = fromIntegral numberOfSessionsMSB * 256
                        + fromIntegral numberOfSessionsLSB,
                diFirstTrackNumberInLastSession
                    = fromIntegral firstTrackNumberInLastSessionMSB * 256
                        + fromIntegral firstTrackNumberInLastSessionLSB,
                diLastTrackNumberInLastSession
                    = fromIntegral lastTrackNumberInLastSessionMSB * 256
                        + fromIntegral lastTrackNumberInLastSessionLSB,
                diUnrestrictedUse
                    = bit2bool unrestrictedUse,
                diBackGroundFormatStatus            = backGroundFormatStatus,
                diDiscType                          = discType,
                diDiscIdentification
                    = if bit2bool discIdentificationValid then
                        Just discIdentification
                      else
                        Nothing,
                diLastSessionLeadinStartAddress     =
                    lastSessionLeadinStartAddress,
                diLastPossibleLeadoutStartAddress   =
                    lastPossibleLeadoutStartAddress,
                diDiscBarCode
                    = if bit2bool discBarCodeValid then
                        Just discBarCode
                      else
                        Nothing,
                diDiscApplicationCode
                    = if bit2bool discApplicationCodeValid then
                        Just discApplicationCode
                      else
                        Nothing,
                diNumberOfOpcTables                 = numberOfOpcTables,
                diOpcTableEntries                   = []
            }

readDiscInformation :: ScsiDevice -> IO DiscInformation
readDiscInformation device = do
    Left res <- commandR device 65536
                    [0x51,0x00,0x00,0x00,0x00,0x00,0x00,0xFF,0xFF,0x00]
    return (runParser parseDiscInformation res)
