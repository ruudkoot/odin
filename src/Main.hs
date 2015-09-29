module Main where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import System.Environment

import Parser
import SCSI
import SCSI.Command
import SCSI.MMC

-- | Main

main :: IO ()
main = do
    [filePath] <- getArgs
    withScsiDevice filePath doScsiStuff
    return ()

doScsiStuff :: ScsiDevice -> IO ()
doScsiStuff device = do

    header "INQUIRE"
    inq <- inquiry device
    putStrLn (show inq)

    case deviceType inq of
        MultiMedia -> do

            header "READ DISC INFORMATION"
            di <- readDiscInformation device
            putStrLn (show di)
            
            header "READ TOC/PMA/ATIP"
            res <- readFormattedToc device LBA 0
            putStrLn (show res)

        _ -> return ()
    
header :: String -> IO ()
header h = putStrLn $ "== " ++ h ++ " " ++ replicate (76 - length h) '='
