module SCSI.Sense (
    ResponseCode(..),
    SenseData(..),
    parseSenseData,
    decodeSenseData
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Word
import Parser

data ResponseCode = Current | Deferred
  deriving Show
  
data SenseKey
    = NO_SENSE          -- 0h
    | RECOVERED_ERROR   -- 1h
    | NOT_READY         -- 2h
    | MEDIUM_ERROR      -- 3h
    | HARDWARE_ERROR    -- 4h
    | ILLEGAL_REQUEST   -- 5h
    | UNIT_ATTENTION    -- 6h
    | DATA_PROTECT      -- 7h
    | BLANK_CHECK       -- 8h
    | VENSOR_SPECIFIC   -- 9h
    | COPY_ABORTED      -- Ah
    | ABORTED_COMMAND   -- Bh
    | EQUAL             -- Ch
    | VOLUME_OVERFLOW   -- Dh
    | MISCOMPARE        -- Eh
    | RESERVED          -- Fh
  deriving (Enum, Show)

data SenseData = SenseData {
    responseCode                    :: ResponseCode,
    senseKey                        :: SenseKey,
    incorrectLengthIndicator        :: Bool,
    endOfMedium                     :: Bool,
    fileMark                        :: Bool,
    information                     :: Maybe Word64,
    commandSpecificInformation      :: Maybe Word64,
    additionalSenseCode             :: Maybe Word8,
    additionalSenseCodeQualifier    :: Maybe Word8,
    fieldReplacableUnitCode         :: Maybe Word8,
    senseKeySpecific                :: Maybe (Word8, Word8, Word8),
    additionalSenseBytes            :: [Word8]
} deriving Show

parseSenseData :: Parser SenseData
parseSenseData = do
    [valid, responseCode]           <- bits8 [1,7]
    _obsolete                       <- word8
    [fileMark, endOfMedium, incorrectLengthIndicator, reserved, senseKey]
                                    <- bits8 [1,1,1,1,4]
    information                     <- word64BE
    additionalSenseLength           <- fromIntegral <$> word8
    commandSpecificInformation      <- word64BE
    additionalSenseCode             <- word8
    additionalSenseCodeQualifier    <- word8
    fieldReplacableUnitCode         <- word8
    [sksv, sks1]                    <- bits8 [1,7]
    sks2                            <- word8
    sks3                            <- word8
    additionalSenseBytes            <- replicateM (additionalSenseLength - 10) word8
    
    return $ SenseData {
        responseCode                    = case responseCode of
                                            0x70 -> Current
                                            0x71 -> Deferred
                                            0x72 -> Current
                                            0x73 -> Deferred,
        senseKey                        = toEnum (fromIntegral senseKey),
        incorrectLengthIndicator        = bit2bool incorrectLengthIndicator,
        endOfMedium                     = bit2bool endOfMedium,
        fileMark                        = bit2bool fileMark,
        information                     = if bit2bool valid then
                                            Just information
                                          else
                                            Nothing,
        commandSpecificInformation      = if additionalSenseLength >= 4 then
                                            Just commandSpecificInformation
                                          else
                                            Nothing,
        additionalSenseCode             = if additionalSenseLength >= 5 then
                                            Just additionalSenseCode
                                          else
                                            Nothing,
        additionalSenseCodeQualifier    = if additionalSenseLength >= 6 then
                                            Just additionalSenseCodeQualifier
                                          else
                                            Nothing,
        fieldReplacableUnitCode         = if additionalSenseLength >= 7 then
                                            Just fieldReplacableUnitCode
                                          else
                                            Nothing,
        senseKeySpecific                = if additionalSenseLength >= 8
                                                && bit2bool sksv then
                                            Just (sks1, sks2, sks3)
                                          else
                                            Nothing,
        additionalSenseBytes            = additionalSenseBytes
    }

decodeSenseData :: SenseData -> String
decodeSenseData senseData = undefined
