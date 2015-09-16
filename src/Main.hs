module Main where

import qualified Data.ByteString as BS
import Data.Char
import System.Environment

import SCSI

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
doScsiStuff scsiDevice = do

    let blockSize = 512
    let dataIn    = BS.replicate (256 * blockSize) 0x00

    res <- scsiCommand scsiDevice dataIn [0x12, 0x00, 0x00, 0x00, 0xff, 0x00]

    case res of
        Right err -> putStrLn $ "FAILED: " ++ show err
        Left dataOut -> do
            let vendor  = bytesToString dataOut  8  8
            let product = bytesToString dataOut 16 16
            let version = bytesToString dataOut 32  4

            putStrLn $ "Vendor : " ++ vendor
            putStrLn $ "Product: " ++ product
            putStrLn $ "Version: " ++ version

    return ()
