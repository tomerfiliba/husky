{-# LANGUAGE ScopedTypeVariables #-}

-- module Construct where

import Prelude hiding (const, repeat)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as BIN 
import Data.Bits (Bits, bitSize, shiftR, shiftL)
import qualified Data.Map as Map
import qualified Data.Char as Char 
import Data.Map ((!))
import qualified Numeric


data PU a = PU { appP :: (a, LBS.ByteString) -> LBS.ByteString,
                 appU :: LBS.ByteString -> (a, LBS.ByteString) }

pack :: PU a -> a -> LBS.ByteString
pack pa value = appP pa (value, LBS.empty)

unpack :: PU a -> LBS.ByteString -> a
unpack pa stream = fst (appU pa stream)

bytes :: Int64 -> PU LBS.ByteString
bytes n = PU (uncurry LBS.append) (LBS.splitAt n)

primitive :: forall a. (Bounded a, Bits a, BIN.Binary a) => PU a
primitive = PU (\ (n, s) -> LBS.append (BIN.encode n) s)
               (\ s -> (BIN.decode s, LBS.drop (fromIntegral size) s))
               where
                 size = (bitSize (maxBound :: a)) `shiftR` 3

word8  = primitive :: PU Word8
word16 = primitive :: PU Word16
word32 = primitive :: PU Word32
word64 = primitive :: PU Word64
int8   = primitive :: PU Int8
int16  = primitive :: PU Int16
int32  = primitive :: PU Int32
int64  = primitive :: PU Int64

lift :: a -> PU a
lift x = PU snd (\s -> (x,s))

tup2 :: PU a -> PU b -> PU (a,b)
tup2 pa pb = PU (\((va,vb),s) -> let s' = appP pb (vb, s)
                                 in appP pa (va, s'))
                (\s -> let (va, s') = appU pa s
                       in let (vb, s'') = appU pb s'
                          in ((va, vb), s''))

convert :: (a->b) -> (b->a) -> PU a -> PU b
convert f f' pa = PU (\(vb, s) -> appP pa (f' vb, s))
                     (\s -> let (va, s') = appU pa s
                            in (f va, s'))

tup3 :: PU a -> PU b -> PU c -> PU (a,b,c)
tup3 pa pb pc = convert (\((a,b),c) -> (a,b,c)) (\(a,b,c) -> ((a,b),c)) $ tup2 (tup2 pa pb) pc

tup4 :: PU a -> PU b -> PU c -> PU d -> PU (a,b,c,d)
tup4 pa pb pc pd = convert (\((a,b,c),d) -> (a,b,c,d)) (\(a,b,c,d) -> ((a,b,c),d)) $ tup2 (tup3 pa pb pc) pd

tup5 :: PU a -> PU b -> PU c -> PU d -> PU e -> PU (a,b,c,d,e)
tup5 pa pb pc pd pe = convert (\((a,b,c,d),e) -> (a,b,c,d,e)) (\(a,b,c,d,e) -> ((a,b,c,d),e)) $ tup2 (tup4 pa pb pc pd) pe

(@.) :: PU a -> PU b -> PU (a,b)
(@.) = tup2

(@|) :: PU a -> (a->b, b->a) -> PU b
(@|) pa (f, f') = convert f f' pa

repeat :: PU a -> Int -> PU [a]
repeat pa 0 = lift []
repeat pa n = convert (\(x,xs) -> (x:xs)) (\(x:xs) -> (x,xs)) (tup2 pa (repeat pa (n-1)))

{-
instance Monad PU where
    pa >>= fpb = convert snd fpb 
    return = lift

main = do
        l <- word8
        repeat word8 l

-}

{-
pipe :: PU a -> PU b -> PU c
pipe pa pb = PU (\((va,vb),s) -> 5)
                (\s -> let (va, s') = appU pa s
                       in let 
-}


{-
pair :: PU a -> PU b -> PU (a,b)
pair pa pb = sequ fst pa (\ a ->
               sequ snd pb (\ b ->
                 const (a,b)))

triple :: PU a -> PU b -> PU c -> PU (a,b,c)
triple pa pb pc = sequ (\ (x,y,z) -> x) pa (\a ->
                    sequ (\ (x,y,z) -> y) pb (\b ->
                      sequ (\ (x,y,z) -> z) pc (\c ->
                        const (a,b,c))))

quad :: PU a -> PU b -> PU c -> PU d -> PU (a,b,c,d)
quad pa pb pc pd = sequ (\ (x,y,z,w) -> x) pa (\a ->
                     sequ (\ (x,y,z,w) -> y) pb (\b ->
                       sequ (\ (x,y,z,w) -> z) pc (\c ->
                         sequ (\ (x,y,z,w) -> w) pd (\d ->
                           const (a,b,c,d) ))))

convert :: (a->b) -> (b->a) -> PU a -> PU b
convert f f' pa = sequ f' pa (const . f)

fixedList :: PU a -> Int -> PU [a]
fixedList pa 0 = const []
fixedList pa n = convert (\(a,b) -> a:b) (\(a:b) -> (a,b)) (pair pa (fixedList pa (n-1)))

convertIntegral :: (Integral a, Integral b) => PU a -> PU b
convertIntegral = convert fromIntegral fromIntegral

list :: PU a -> PU [a]
list pa = sequ length (convertIntegral word32) (fixedList pa)

data IPv4 = IPv4 Word8 Word8 Word8 Word8 deriving (Eq, Ord, Show)

ipv4 :: PU IPv4
ipv4 = convert (\(a:b:c:d:_) -> IPv4 a b c d) 
               (\(IPv4 a b c d) -> [a,b,c,d]) 
               (fixedList word8 4)

validate :: forall a. (Eq a) => PU a -> a -> PU ()
validate pa val = convert validator (\_ -> val) pa
                  where
                    validator :: a -> ()
                    validator x = if x == val then () else error "basa"

magic :: LBS.ByteString -> PU ()
magic str = validate (bytes (fromIntegral $ LBS.length str)) str

reverseMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
reverseMap m = Map.fromList (map (\(x,y) -> (y,x)) (Map.toList m))

symmMapping :: (Ord k, Ord v) => PU k -> Map.Map k v -> PU v
symmMapping pa m = convert (\x -> m ! x) (\x -> (reverseMap m) ! x) pa
-}

{-
instance monad PU where
  return a = const a
  x >>= f = sequ id 

crap = do
    len <- convertIntegral word32
    lst <- fixedList word8 len
    return lst
-}

data BMPVersion = BMPv1 | BMPv2 deriving (Eq, Ord, Show)


main :: IO ()
main = do
        p <- return $ tup2 word32 word16
        c <- return $ convert (Char.chr . fromIntegral) (fromIntegral . Char.ord) word8
        rc <- return $ repeat c
        
        p2 <- return $ word8 @. word16 @. (word32 @| ((`shiftL` 1), (`shiftR` 1))) 
        
        
        print $ LBS.unpack $ pack p (65, 66)
        print $ unpack p $ LBS.pack [0,0,0,1,0,2]
        print $ pack c 'Z'
        print $ unpack c $ LBS.pack [90]
        print $ LBS.unpack $ pack (repeat word16 2) [13,14]
        print $ unpack p2 $ LBS.pack [0, 1, 1, 0,0,0,7]
        
        {- 
        print $ LBS.unpack $ pack (tup5 word8 word8 word8 word8 word8) (1,2,3,4,5) 
        print $ unpack (tup5 word8 word8 word8 word8 word8) $ LBS.pack [1,2,3,4,5] 
    
        LBS.putStrLn $ pack (fixedList word32 2) [65, 66, 67]
        print $ unpack (fixedList word16 2) (LBS.pack [0, 65, 0, 66, 0, 67])
        LBS.putStrLn $ pack (list word8) [65,66,67,68,69]
        LBS.putStrLn $ pack ipv4 (IPv4 65 66 67 68)
        print $ unpack ipv4 (LBS.pack [127, 0, 0, 1])
           
        m <- return $ LBS.pack [65, 66, 67]
        LBS.putStrLn $ pack (magic m) ()
        print $ unpack (magic m) (LBS.pack [65, 66, 67, 68])  -- ()
        print $ unpack (magic m) (LBS.pack [66, 67, 68, 69])  -- error "basa"
            
        bmpver <- return $ word8 `symmMapping` (Map.fromList [(17, BMPv1), (31, BMPv2)])
        print $ unpack bmpver (LBS.pack [31])
        print $ pack bmpver BMPv1 
        -}


