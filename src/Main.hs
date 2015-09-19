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
    [filePath] <- getArgs
    withScsiDevice filePath doScsiStuff
    return ()

bytesToString :: BS.ByteString -> Int -> Int -> String
bytesToString buffer start len
    = map (chr . fromIntegral) $ BS.unpack $ BS.take len $ BS.drop start buffer

doScsiStuff :: ScsiDevice -> IO ()
doScsiStuff device = do

    header "INQUIRE"
    inquire             device

    header "READ DISC INFORMATION"
    di <- readDiscInformation device
    putStrLn (show di)
    
    return ()
    
header :: String -> IO ()
header h = putStrLn $ "== " ++ h ++ " " ++ replicate (76 - length h) '='
    
-- * INQUIRE
    
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
