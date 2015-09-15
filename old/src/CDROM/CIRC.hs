{-# LANGUAGE    NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fcontext-stack=100       #-}

module CDROM.CIRC where

import Control.Arrow

-- | Types that can flow throught the encoder.

class Electrical a where
    neutral :: a
    invert  :: a -> a
    
instance Electrical Integer where
    neutral = 666
    invert  = negate

-- | Attempt 1

input  = [4,7,6]
input' = (input, (input, (input, input)))

delay n xs = replicate n neutral ++ xs


stageDelay                    = id *** delay 2 *** id *** delay 2
stagePermute (a, (b, (c, d))) = (a, (b, (d, c)))
stageParity  (a, (b, (c, d))) = (a, (b, (c, (d, zipWith (+) a b))))
stageDelay2                   = id *** delay 1 *** delay 2 *** delay 3 *** delay 4


-- | Attempt 2

encoderDelay2 =     delay 2 *** delay 2 *** delay 2 *** delay 2
                ***
                    id      *** id      *** id      *** id
                ***    
                    delay 2 *** delay 2 *** delay 2 *** delay 2
                ***
                    id      *** id      *** id      *** id
                ***
                    delay 2 *** delay 2 *** delay 2 *** delay 2
                ***
                    id      *** id      *** id      *** id

interleave (a, (b, (c, (d, (e, (f, (g, (h,
           (i, (j, (k, (l, (m, (n, (o, (p,
           (q, (r, (s, (t, (u, (v, (w,  x
           )))))))))))))))))))))))         = (a, (b, (i, (j, (q, (r, (c, (d,
                                             (k, (l, (s, (t, (e, (f, (m, (n,
                                             (u, (v, (g, (h, (o, (p, (w,  x
                                             )))))))))))))))))))))))

c2encoder :: Electrical a => ([a], ([a], ([a], ([a],
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], [a])))))))))))))))))))))))  -> ([a], ([a], ([a], ([a],                
                                                                              ([a], ([a], ([a], ([a], 
                                                                              ([a], ([a], ([a], ([a], 
                                                                              ([a], ([a], ([a], ([a], 
                                                                              ([a], ([a], ([a], ([a], 
                                                                              ([a], ([a], ([a], ([a],
                                                                              ([a], ([a], ([a], [a])))))))))))))))))))))))))))

c2encoder (a, (b, (c, (d, (e, (f, (g, (h,
          (i, (j, (k, (l, (m, (n, (o, (p,
          (q, (r, (s, (t, (u, (v, (w,  x
          )))))))))))))))))))))))         = (a, (b, (c, (d, (e, (f, (g, (h,
                                            (i, (j, (k, (l, (repeat neutral,
                                            (repeat neutral, (repeat neutral,
                                            (repeat neutral, (m, (n, (o, (p,
                                            (q, (r, (s, (t, (u, (v, (w,  x
                                            )))))))))))))))))))))))))))

encoderDelayUnequal = let d = 4
                       in     id           *** delay ( 1*d) *** delay ( 2*d) *** delay ( 3*d)
                          ***
                              delay ( 4*d) *** delay ( 5*d) *** delay ( 6*d) *** delay ( 7*d)
                          ***
                              delay ( 8*d) *** delay ( 9*d) *** delay (10*d) *** delay (11*d)
                          ***
                              delay (12*d) *** delay (13*d) *** delay (14*d) *** delay (15*d)
                          ***
                              delay (16*d) *** delay (17*d) *** delay (18*d) *** delay (19*d)
                          ***
                              delay (20*d) *** delay (21*d) *** delay (22*d) *** delay (23*d)    
                          ***
                              delay (24*d) *** delay (25*d) *** delay (26*d) *** delay (27*d)
                              
c1encoder :: Electrical a => ([a], ([a], ([a], ([a],
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], ([a], 
                             ([a], ([a], ([a], [a])))))))))))))))))))))))))))  -> ([a], ([a], ([a], ([a],                
                                                                                  ([a], ([a], ([a], ([a], 
                                                                                  ([a], ([a], ([a], ([a], 
                                                                                  ([a], ([a], ([a], ([a], 
                                                                                  ([a], ([a], ([a], ([a], 
                                                                                  ([a], ([a], ([a], ([a], 
                                                                                  ([a], ([a], ([a], ([a],
                                                                                  ([a], ([a], ([a], [a])))))))))))))))))))))))))))))))
c1encoder (a, (b, (c, (d, (e, (f, (g, (h,
          (i, (j, (k, (l, (m, (n, (o, (p,
          (q, (r, (s, (t, (u, (v, (w, (x,
          (y, (z, (a', b')
          ))))))))))))))))))))))))))      = (a, (b, (c, (d, (e, (f, (g, (h,
                                            (i, (j, (k, (l, (m, (n, (o, (p,
                                            (q, (r, (s, (t, (u, (v, (w, (x,
                                            (y, (z, (a', (b',
                                            (repeat neutral, (repeat neutral,
                                            (repeat neutral,  repeat neutral
                                            )))))))))))))))))))))))))))))))

encoderDelay1 =     delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id
                ***
                    delay 1 *** id *** delay 1 *** id

invertParity =     id     *** id     *** id     *** id
               ***
                   id     *** id     *** id     *** id
               ***
                   id     *** id     *** id     *** id
               ***
                   map invert *** map invert *** map invert *** map invert
               ***
                   id     *** id     *** id     *** id
               ***
                   id     *** id     *** id     *** id
               ***
                   id     *** id     *** id     *** id
               ***
                   map invert *** map invert *** map invert *** map invert

circEncoder = encoderDelay2 >>> interleave >>> c2encoder >>> encoderDelayUnequal
                            >>> c1encoder >>> encoderDelay1 >>> invertParity
                            
-- | Test input
                            
constantFrame n = (n, (n, (n, (n, (n, (n, (n, (n, (n, (n, (n, (n, 
                  (n, (n, (n, (n, (n, (n, (n, (n, (n, (n, (n,  n)))))))))))))))))))))))
                  
testInput :: ([Integer], ([Integer], ([Integer], ([Integer], 
             ([Integer], ([Integer], ([Integer], ([Integer],
             ([Integer], ([Integer], ([Integer], ([Integer], 
             ([Integer], ([Integer], ([Integer], ([Integer], 
             ([Integer], ([Integer], ([Integer], ([Integer], 
             ([Integer], ([Integer], ([Integer], ([Integer]))))))))))))))))))))))))
testInput = (n 0, (n 1, (n 2, (n 3, (n 4, (n 5, (n 6, (n 7, (n 8, (n 9, (n 10, (n 11, 
            (n 12, (n 13, (n 14, (n 15, (n 16, (n 17, (n 18, (n 19, (n 20, (n 21, (n 22,  n 23)))))))))))))))))))))))
            where n s = {-takeWhile (<100000) $-} iterate (+24) s
            
ti :: [Integer] -> [(Integer, (Integer, (Integer, (Integer, 
                    (Integer, (Integer, (Integer, (Integer, 
                    (Integer, (Integer, (Integer, (Integer, 
                    (Integer, (Integer, (Integer, (Integer, 
                    (Integer, (Integer, (Integer, (Integer, 
                    (Integer, (Integer, (Integer,  Integer)))))))))))))))))))))))]
ti (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:zs) = (a, (b, (c, (d,
                                                          (e, (f, (g, (h,
                                                          (i, (j, (k, (l,
                                                          (m, (n, (o, (p,
                                                          (q, (r, (s, (t,
                                                          (u, (v, (w,  x))))))))))))))))))))))) : ti zs
                                                          
it ((a, (b, (c, (d,
      (e, (f, (g, (h,
      (i, (j, (k, (l,
      (m, (n, (o, (p,
      (q, (r, (s, (t,
      (u, (v, (w,  x))))))))))))))))))))))) : zs) = (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:(it zs))
                                                          
uz :: [(Integer, (Integer, (Integer, (Integer, 
       (Integer, (Integer, (Integer, (Integer, 
       (Integer, (Integer, (Integer, (Integer, 
       (Integer, (Integer, (Integer, (Integer, 
       (Integer, (Integer, (Integer, (Integer, 
       (Integer, (Integer, (Integer,  Integer)))))))))))))))))))))))] -> ([Integer], ([Integer], ([Integer], ([Integer], 
                                                                         ([Integer], ([Integer], ([Integer], ([Integer],
                                                                         ([Integer], ([Integer], ([Integer], ([Integer], 
                                                                         ([Integer], ([Integer], ([Integer], ([Integer], 
                                                                         ([Integer], ([Integer], ([Integer], ([Integer], 
                                                                         ([Integer], ([Integer], ([Integer], ([Integer]))))))))))))))))))))))))
uz [] = ([], ([], ([], ([], 
        ([], ([], ([], ([], 
        ([], ([], ([], ([], 
        ([], ([], ([], ([], 
        ([], ([], ([], ([], 
        ([], ([], ([],  [])))))))))))))))))))))))
uz ((a, (b, (c, (d,
    (e, (f, (g, (h,
    (i, (j, (k, (l,
    (m, (n, (o, (p,
    (q, (r, (s, (t,
    (u, (v, (w,  x))))))))))))))))))))))) : zs) = let (as, (bs, (cs, (ds, (es, (fs, (gs, (hs, (is, (js, (ks, (ls, (ms, (ns, (os, (ps, (qs, (rs, (ss, (ts, (us, (vs, (ws,  xs))))))))))))))))))))))) = uz zs
                                                   in (a:as, (b:bs, (c:cs, (d:ds,
                                                      (e:es, (f:fs, (g:gs, (h:hs,
                                                      (i:is, (j:js, (k:ks, (l:ls,
                                                      (m:ms, (n:ns, (o:os, (p:ps,
                                                      (q:qs, (r:rs, (s:ss, (t:ts,
                                                      (u:us, (v:vs, (w:ws,  x:xs)))))))))))))))))))))))

zp :: ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer],  [Integer]))))))))))))))))))))))))))))))) -> [(Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer,  Integer)))))))))))))))))))))))))))))))]
zp ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([],  []))))))))))))))))))))))))))))))) = []
zp (a:as, (b:bs, (c:cs, (d:ds,
   (e:es, (f:fs, (g:gs, (h:hs,
   (i:is, (j:js, (k:ks, (l:ls,
   (m:ms, (n:ns, (o:os, (p:ps,
   (q:qs, (r:rs, (s:ss, (t:ts,
   (u:us, (v:vs, (w:ws, (x:xs,
   (y:ys, (z:zs, (a':a's, (b':b's,
   (c':c's, (d':d's, (e':e's, (f':f's)))))))))))))))))))))))))))))))) = (a, (b, (c, (d, 
                                                                        (e, (f, (g, (h,
                                                                        (i, (j, (k, (l,
                                                                        (m, (n, (o, (p,
                                                                        (q, (r, (s, (t,
                                                                        (u, (v, (w, (x,
                                                                        (y, (z, (a', (b',
                                                                        (c', (d', (e',  f'))))))))))))))))))))))))))))))) : zp (as, (bs, (cs, (ds,
                                                                                                                               (es, (fs, (gs, (hs,
                                                                                                                               (is, (js, (ks, (ls,
                                                                                                                               (ms, (ns, (os, (ps,
                                                                                                                               (qs, (rs, (ss, (ts,
                                                                                                                               (us, (vs, (ws, (xs,
                                                                                                                               (ys, (zs, (a's, (b's,
                                                                                                                               (c's, (d's, (e's,  f's)))))))))))))))))))))))))))))))
zp (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_,  _))))))))))))))))))))))))))))))) = error "moo!"


pz :: ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer], ([Integer],
      ([Integer], ([Integer], ([Integer],  [Integer]))))))))))))))))))))))) -> [(Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer, (Integer,
                                                                                        (Integer, (Integer, (Integer,  Integer)))))))))))))))))))))))]
pz ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([], ([],
   ([], ([], ([],  []))))))))))))))))))))))) = []
pz (a:as, (b:bs, (c:cs, (d:ds,
   (m:ms, (n:ns, (o:os, (p:ps,
   (q:qs, (r:rs, (s:ss, (t:ts,
   (u:us, (v:vs, (w:ws, (x:xs,
   (y:ys, (z:zs, (a':a's, (b':b's,
   (c':c's, (d':d's, (e':e's, (f':f's)))))))))))))))))))))))) = (a, (b, (c, (d, 
                                                                        (m, (n, (o, (p,
                                                                        (q, (r, (s, (t,
                                                                        (u, (v, (w, (x,
                                                                        (y, (z, (a', (b',
                                                                        (c', (d', (e',  f'))))))))))))))))))))))) : pz (as, (bs, (cs, (ds,
                                                                                                                               (ms, (ns, (os, (ps,
                                                                                                                               (qs, (rs, (ss, (ts,
                                                                                                                               (us, (vs, (ws, (xs,
                                                                                                                               (ys, (zs, (a's, (b's,
                                                                                                                               (c's, (d's, (e's,  f's)))))))))))))))))))))))
pz (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_, (_,
   (_, (_, (_,  _))))))))))))))))))))))) = error "moo!"

-- | Decoder

decoderDelay1 =     id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1
                ***
                    id *** delay 1 *** id *** delay 1

c1decoder (a, (b, (c, (d, (e, (f, (g, (h,
          (i, (j, (k, (l, (m, (n, (o, (p,
          (q, (r, (s, (t, (u, (v, (w, (x,
          (y, (z, (a',(b', (c', (d', (e', f')
          ))))))))))))))))))))))))))))))  = (a, (b, (c, (d, (e, (f, (g, (h,
                                            (i, (j, (k, (l, (m, (n, (o, (p,
                                            (q, (r, (s, (t, (u, (v, (w, (x,
                                            (y, (z, (a', b')))))))))))))))))))))))))))

decoderDelayUnequal = let d = 4
                       in     delay (27*d) *** delay (26*d) *** delay (25*d) *** delay (24*d)
                          ***
                              delay (23*d) *** delay (22*d) *** delay (21*d) *** delay (20*d)
                          ***
                              delay (19*d) *** delay (18*d) *** delay (17*d) *** delay (16*d)
                          ***
                              delay (15*d) *** delay (14*d) *** delay (13*d) *** delay (12*d)
                          ***
                              delay (11*d) *** delay (10*d) *** delay ( 9*d) *** delay ( 8*d)
                          ***
                              delay ( 7*d) *** delay ( 6*d) *** delay ( 5*d) *** delay ( 4*d)    
                          ***
                              delay ( 3*d) *** delay ( 2*d) *** delay ( 1*d) ***           id

c2decoder (a, (b, (c, (d, (e, (f, (g, (h,
          (i, (j, (k, (l, (_, (_, (_, (_,
          (m, (n, (o, (p, (q, (r, (s, (t,
          (u, (v, (w, x))))))))))))))))))))))))))) = (a, (b, (c, (d, (e, (f, (g, (h,
                                                     (i, (j, (k, (l, (m, (n, (o, (p,
                                                     (q, (r, (s, (t, (u, (v, (w,  x
                                                     )))))))))))))))))))))))

deinterleave (a, (b, (c, (d, (e, (f, (g, (h,
             (i, (j, (k, (l, (m, (n, (o, (p,
             (q, (r, (s, (t, (u, (v, (w,  x
             )))))))))))))))))))))))         = (a, (b, (g, (h, (m, (n, (s, (t,
                                                     (c, (d, (i, (j, (o, (p, (u, (v,
                                                     (e, (f, (k, (l, (q, (r, (w,  x
                                                     )))))))))))))))))))))))

decoderDelay2 =     id      *** id      *** id      *** id
                ***
                    delay 2 *** delay 2 *** delay 2 *** delay 2
                ***
                    id      *** id      *** id      *** id
                ***    
                    delay 2 *** delay 2 *** delay 2 *** delay 2
                ***
                    id      *** id      *** id      *** id
                ***
                    delay 2 *** delay 2 *** delay 2 *** delay 2

circDecoder = decoderDelay1 >>> invertParity >>> c1decoder >>> decoderDelayUnequal >>> c2decoder >>> deinterleave >>> decoderDelay2

-- | main

main = print main'

main' = let e = {-zp $ -}circEncoder (uz $ ti [1..])
            d = pz $ circDecoder ({-uz-} e)
         in length $ take 10000000 $ it $ d
