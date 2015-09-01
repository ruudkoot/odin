module System.IO.SCSI () where

import Control.Monad

import System.IO.SCSI.SG

type SCSIState  = Bool
type SCSIReturn = Bool

type SCSIIO = StateT SCSIState IO SCSIReturn




