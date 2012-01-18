{-# LANGUAGE ScopedTypeVariables #-}

-- module Construct where

import Prelude hiding (const, repeat)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as BIN 
import Data.Bits (Bits, bitSize, shiftR)
import qualified Data.Map as Map
import Data.Map ((!))

data PU a = PU { appP :: (a, LBS.ByteString) -> LBS.ByteString,
                 appU :: LBS.ByteString -> (a, LBS.ByteString) }

pack :: PU a -> a -> LBS.ByteString
pack p value = appP p (value, LBS.empty)

unpack :: PU a -> LBS.ByteString -> a
unpack p stream = fst (appU p stream)

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

const :: a -> PU a
const x = PU snd (\s -> (x,s))

sequ :: (b -> a) -> PU a -> (a -> PU b) -> PU b
sequ f pa k = PU (\ (b,s) -> let a = f b
                                 pb = k a
                             in appP pa (a, appP pb (b,s)))
                 (\ s -> let (a,s') = appU pa s
                             pb = k a
                         in appU pb s')

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
    --LBS.putStrLn $ pack word32 65
    --LBS.putStrLn $ pack (fixedList word32 2) [65, 66, 67]
    --print $ unpack (fixedList word16 2) (LBS.pack [0, 65, 0, 66, 0, 67])
    --LBS.putStrLn $ pack (list word8) [65,66,67,68,69]
    --LBS.putStrLn $ pack ipv4 (IPv4 65 66 67 68)
    --print $ unpack ipv4 (LBS.pack [127, 0, 0, 1])
    
    m <- return $ LBS.pack [65, 66, 67]
    LBS.putStrLn $ pack (magic m) ()
    print $ unpack (magic m) (LBS.pack [65, 66, 67, 68])  -- ()
    --print $ unpack (magic m) (LBS.pack [66, 67, 68, 69])  -- error "basa"
    
    bmpver <- return $ word8 `symmMapping` (Map.fromList [(17, BMPv1), (31, BMPv2)])
    print $ unpack bmpver (LBS.pack [31])
    print $ pack bmpver BMPv1


