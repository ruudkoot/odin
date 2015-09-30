module Parser (
    Parser,
    bit2bool,
    bigEndian,
    runParser,
    bits8,
    word8,
    word16BE,
    word32BE,
    word64BE
) where

import Prelude hiding (drop, head, tail)

import Control.Applicative
import Data.Bits
import Data.ByteString hiding (reverse)
import Data.Word

data Parser t = Parser { unP :: ByteString -> (t, ByteString) }

instance Functor Parser where
    fmap f (Parser p) = Parser $
        \x -> let (t, r) = p x in (f t, r)

instance Applicative Parser where
    pure x = Parser (\a -> (x, a))
    Parser p <*> Parser q = Parser $
        \a -> let (f, b) = p a in let (x, c) = q b in (f x, c)
        
instance Monad Parser where
    return = pure
    Parser p >>= k = Parser $ \a -> let (x,b) = p a in unP (k x) b

bit2bool :: Word8 -> Bool
bit2bool 0 = False
bit2bool 1 = True

bigEndian :: Word32 -> [Word8]
bigEndian w =
    [ fromIntegral $ shiftR w 24
    , fromIntegral $ shiftR w 16
    , fromIntegral $ shiftR w  8
    , fromIntegral $        w
    ]

runParser :: Parser a -> ByteString -> a
runParser (Parser p) = fst . p

bits8 :: [Int] -> Parser [Word8]
bits8 xs | sum xs == 8 = Parser $ \bs ->
            (reverse $ bits8' (reverse xs) (head bs), tail bs)
  where bits8' :: [Int] -> Word8 -> [Word8]
        bits8' [] _     = []
        bits8' (x:xs) w = w .&. (2^x - 1) : bits8' xs (shiftR w x)

word8 :: Parser Word8
word8 = Parser $ \bs ->
    ( index bs 0
    , drop 1 bs
    )

word16BE :: Parser Word16
word16BE = Parser $ \bs ->
    ( fromIntegral (index bs 0) * 256
    + fromIntegral (index bs 1)
    , drop 2 bs
    )

word32BE :: Parser Word32
word32BE = Parser $ \bs ->
    ( fromIntegral (index bs 0) * 256 * 256 * 256
    + fromIntegral (index bs 1) * 256 * 256
    + fromIntegral (index bs 2) * 256
    + fromIntegral (index bs 3)
    , drop 4 bs
    )

word64BE :: Parser Word64
word64BE = Parser $ \bs ->
    ( fromIntegral (index bs 0) * 256 * 256 * 256 * 256 * 256 * 256 * 256
    + fromIntegral (index bs 1) * 256 * 256 * 256 * 256 * 256 * 256
    + fromIntegral (index bs 2) * 256 * 256 * 256 * 256 * 256
    + fromIntegral (index bs 3) * 256 * 256 * 256 * 256
    + fromIntegral (index bs 4) * 256 * 256 * 256
    + fromIntegral (index bs 5) * 256 * 256
    + fromIntegral (index bs 6) * 256
    + fromIntegral (index bs 7)
    , drop 8 bs
    )
