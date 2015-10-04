module SCSI.Command.TestUnitReady (
    testUnitReady
) where

import SCSI

testUnitReady :: ScsiDevice -> IO (Either () ScsiError)
testUnitReady device = do
    commandN device $ [0x00,0x00,0x00,0x00,0x00,0x00]
