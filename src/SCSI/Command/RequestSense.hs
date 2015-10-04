module SCSI.Command.RequestSense (
    requestSense
) where

import Parser
import SCSI
import SCSI.Sense

requestSense :: ScsiDevice -> IO SenseData
requestSense device = do
    Left res <- commandR device 252 [0x03,0x00,0x00,0x00,252,0x00]
    return (runParser parseSenseData res)
