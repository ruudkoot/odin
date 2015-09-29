module SCSI.MMC.Types (
    LBA,
    lba,
    MSF,
    msf,
    Address(..),
    toLBA,
    toMSF
) where

import Common

--  LBA START   MSF START       LBA END     MSF END         COMMENTS
--  -451150     00:00:00    -   -151        99:59:74        "Overburn"
--              A0:00:00    -                               lead-out?
--     -150     00:00:00    -   404849      89:59:74        lead-in + data
--        0     00:02:00                                    data

data LBA = LBA Int
  deriving Eq

lba :: Int -> LBA
lba n | n < (-451150) = error "lba: too small"
      | n >   404849  = error "lba: too large"
      | otherwise     = LBA n 

data MSF = MSF Int Int Int
  deriving Eq

msf :: Int -> Int -> Int -> MSF
msf m s f
    | between 0 99 m && between 0 59 s && between 0 74 f
        = MSF m s f
    | otherwise
        = error $ "msf: out of range (" ++ show (MSF m s f) ++ ")"

data Address
    = AddressLBA LBA
    | AddressMSF MSF

instance Show LBA where
    show (LBA n) = show n

instance Show MSF where
    show (MSF m s f)
        = showDigits 2 m ++ ":" ++ showDigits 2 s ++ ":" ++ showDigits 2 f
    
instance Show Address where
    show (AddressLBA lba) = "LBA " ++ show lba
    show (AddressMSF msf) = "MSF " ++ show msf

toLBA :: Address -> LBA
toLBA (AddressLBA lba)
    = lba
toLBA (AddressMSF (MSF m s f))
    | between  0 89 m && between 0 59 s && between 0 74 f
        = lba $ (m * 60 + s) * 75 + f -    150
    | between 90 99 m && between 0 59 s && between 0 74 f
        = lba $ (m * 60 + s) * 75 + f - 450150
    | otherwise
        = error "toLBA: MSF out of range"

toMSF :: Address -> MSF
toMSF (AddressLBA (LBA n))
    | between (-150) 404849 n
        = let m = (n + 150) `div` (60 * 75)
              s = (n + 150 - m * 60 * 75) `div` 75
              f = n + 150 - m * 60 * 75 - s * 75
           in msf m s f
    | between (-451150) (-151) n
        = let m = (n + 450150) `div` (60 * 75)
              s = (n + 450150 - m * 60 * 75) `div` 75
              f = n + 450150 - m * 60 * 75 - s * 75
           in msf m s f
    | otherwise
        = error "toMSF: LBA out of range"
toMSF (AddressMSF msf)
    = msf
