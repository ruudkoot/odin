module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
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
    [filePath, command] <- getArgs
    withScsiDevice filePath $ case command of
        "info"          -> doInfo
        "quickblank"    -> doQuickBlank
        "read"          -> doRead
    return ()

header :: String -> IO ()
header h = putStrLn $ "== " ++ h ++ " " ++ replicate (76 - length h) '='

doInfo :: ScsiDevice -> IO ()
doInfo device = do

    header "TEST UNIT READY"
    res <- testUnitReady device
    putStrLn (show res)

    header "INQUIRE"
    inq <- inquiry device
    putStrLn (show inq)

    case deviceType inq of
        -- FIXME: Linux lies...
        -- MultiMedia -> do
        DirectAccessBlock -> do

            header "READ CAPACITY"
            res <- readCapacity device
            putStrLn (show res)

            header "READ DISC INFORMATION"
            di <- readDiscInformation device
            putStrLn (show di)
            
            header "READ TOC/PMA/ATIP"
            res <- readFormattedToc device LBA 0
            putStrLn (show res)

        _ -> return ()
        
doQuickBlank :: ScsiDevice -> IO ()
doQuickBlank device = do
    putStrLn "start"
    res <- blank device BlankMinimal False
    putStrLn "done"
    putStrLn (show res)
    
doRead :: ScsiDevice -> IO ()
doRead device = do

    capacity <- readCapacity device
    putStrLn (show capacity)
    
    forM [120000 .. logicalBlockAddress capacity - 1] $ \lba -> do
        putStrLn $ show lba ++ "/" ++ show (logicalBlockAddress capacity - 1)
        res <- read10 device lba
        case res of
            Left _ -> return ()
            Right x -> error (show x)
    
    return ()
