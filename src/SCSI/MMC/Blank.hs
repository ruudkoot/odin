module SCSI.MMC.Blank (
    BlankingType(..),
    blank
) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString
import Data.Word
import Parser
import SCSI

data BlankingType
    = BlankDisc
    | BlankMinimal
    | BlankTrack                    Word32
    | UnreserveTrack
    | BlankTrackTail                Word32
    | UncloseLastCompleteSession
    | UncloseLastNonemptySession
  deriving Show

blankingType :: BlankingType -> (Word8, Word32)
blankingType BlankDisc                  = (0, 0)
blankingType BlankMinimal               = (1, 0)
blankingType (BlankTrack track)         = (2, track)
blankingType UnreserveTrack             = (3, 0)
blankingType (BlankTrackTail lba)       = (4, lba)
blankingType UncloseLastCompleteSession = (5, 0)
blankingType UncloseLastNonemptySession = (6, 0)

blank :: ScsiDevice -> BlankingType -> Bool -> IO (Either () ScsiError)
blank device blankType immed = do
    let (blankByte, startWord) = blankingType blankType
    let blankByte' = if immed then blankByte + 0x10 else blankByte
    res <- commandN device $
        [0xA1,blankByte'] ++ bigEndian startWord ++ [0x00,0x00,0x00,0x00,0x00,0x00]
    return res
