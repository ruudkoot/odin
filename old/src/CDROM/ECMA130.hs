module ECMA130 where

type Byte = Int
type Bit  = Bool

-- | Minute-Second-Fraction

data MSF  = MSF { min  :: Byte
                , sec  :: Byte
                , frac :: Byte }
                
instance Show MSF where
    show msf = (show . min $ msf) ++ ":" ++ (show . sec $ msf) ++ ":" ++ (show . frac $ msf)
                
-- | Sectors

data Sector = Mode0 Sync SectorAddress Mode Zero2336
            | Mode1 Sync SectorAddress Mode UserData2048 EDC Intermediate PParity QParity
            | Mode2 Sync SectorAddress Mode UserData2336
            
data Sync          = Sync          Byte Byte Byte Byte Byte Byte Byte Byte Byte Byte Byte Byte
type SectorAddress = MSF
data Intermediate  = Intermediate  Byte Byte Byte Byte Byte Byte Byte Byte

validSync :: Sync -> Bool
validSync (Sync 0x00 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0x00) = True
validSync _                                                                  = False

validIntermediate :: Intermediate -> Bool
validIntermediate (Intermediate 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00) = True
validIntermediate _                                                      = False



-- | 15 Scrambling

data ScrambledSector = ScrambledSector Sync Scrambled  -- 2352 bytes
type Scrambled       = [Byte]                          -- 2340 bytes

leastSignificantBitstream :: [Byte] -> [Bit]
leastSignificantBitstream []     = []
leastSignificantBitstream (b:bs) = lsb b ++ leastSignificantBitstream bs



scramble :: [Byte] -> [Byte]


-- | B Scramble

lfsr :: [Bit]
lfsr = let lfsr' reg = lsb reg : lfsr' (shift reg `bitwiseOr` feedback reg)
        in lfsr' 1
            where lsb      n = n `mod` 2
                  shift    n = n `div` 2
                  feedback n = let r0 = lsb n
                                   r1 = lsb (shift n)
                                in if r0 `xor` r1 then 0x4000 else 0x0000
            


            
-- | Linear feedback shift register
{-
linearFeedbackShiftRegister :: [Int] -> Integer -> [Bit]
linearFeedbackShiftRegister taps state = 

scrambler = linearFeedbackShiftRegister [15, 1, 0] 1
-}
