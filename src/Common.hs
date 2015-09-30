module Common (
    between,
    showDigits
) where

between :: Ord a => a -> a -> a -> Bool
between p q n = p <= n && n <= q

showDigits :: Int -> Int -> String
showDigits d n
    | n < 0     = error "showDigits: too small"
    | otherwise =
        let ss = show n
         in if length ss > d then
                error "showDigits: too large"
            else
                replicate (d - length ss) '0' ++ ss
