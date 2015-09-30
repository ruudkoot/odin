module Main where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import System.Environment

import Parser
import SCSI
import SCSI.MMC

-- | Main

main :: IO ()
main = do
    [filePath, command] <- getArgs
    withScsiDevice filePath $ case command of
        "info"          -> doInfo
        "quickblank"    -> doQuickBlank
    return ()

header :: String -> IO ()
header h = putStrLn $ "== " ++ h ++ " " ++ replicate (76 - length h) '='

doInfo :: ScsiDevice -> IO ()
doInfo device = do

    header "INQUIRE"
    inquire device

    header "READ DISC INFORMATION"
    di <- readDiscInformation device
    putStrLn (show di)
    
    header "READ TOC/PMA/ATIP"
    res <- readFormattedToc device LBA 0
    putStrLn (show res)
    
doQuickBlank :: ScsiDevice -> IO ()
doQuickBlank device = do
    putStrLn "start"
    res <- blank device BlankMinimal False
    putStrLn "done"
    putStrLn (show res)
   
-- * INQUIRE

bytesToString :: BS.ByteString -> Int -> Int -> String
bytesToString buffer start len
    = map (chr . fromIntegral) $ BS.unpack $ BS.take len $ BS.drop start buffer
    
inquire :: ScsiDevice -> IO ()
inquire scsiDevice = do
    Left dataOut <- commandR scsiDevice 512 [0x12, 0x00, 0x00, 0x00, 0xff, 0x00]

    let vendor  = bytesToString dataOut  8  8
    let product = bytesToString dataOut 16 16
    let version = bytesToString dataOut 32  4
    let serial  = bytesToString dataOut 36  8

    putStrLn $ "Vendor : [" ++ vendor  ++ "]"
    putStrLn $ "Product: [" ++ product ++ "]"
    putStrLn $ "Version: [" ++ version ++ "]"
    putStrLn $ "Serial : [" ++ serial  ++ "]"
