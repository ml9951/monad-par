{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Efficient 'Data.Serialize' instance for 'Data.Vector.Storable'
-- vectors. The serialized format is an 'Int32' representing the
-- length of the 'Vector', followed by the raw bytes.
module Data.Vector.Storable.Serialize where

import Control.Applicative

import qualified Data.ByteString.Internal as BS
import Data.Int 
import Data.Serialize (decode, encode, Get, getBytes, putByteString, Serialize(..))
import Data.Vector.Storable ( fromList
--                            , unsafeFromForeignPtr
                            , unsafeFromForeignPtr0
                            , unsafeToForeignPtr0
                            , Vector)
import Data.Vector.Storable.Internal (updPtr)

import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable, sizeOf)

#define TEST
#ifdef TEST
import Test.QuickCheck
#endif

instance forall a. Storable a => Serialize (Vector a) where
  get = do 
    len <- fromIntegral <$> (get :: Get Int32)
    let nbytes = len * sizeOf (undefined :: a)
    bs <- getBytes nbytes
    let (fp, off, _)    = BS.toForeignPtr bs
        fp' | off /= 0  = updPtr (`advancePtr` off) fp
            | otherwise = fp
    return $ unsafeFromForeignPtr0 (castForeignPtr fp') len

  put v = do
    let (fp, len) = unsafeToForeignPtr0 v
        nbytes    = (len * sizeOf (undefined :: a))
        bs        = BS.fromForeignPtr (castForeignPtr fp) 0 nbytes
    put $ ((fromIntegral len) :: Int32)
    putByteString bs

prop_roundtrip :: (Arbitrary a, Eq a, Storable a) => [a] -> Bool
prop_roundtrip xs = (Right xsv) == decode (encode xsv)
  where xsv = fromList xs

prop_roundtripTuple :: --( Arbitrary a, Eq a, Storable a
                       --, Arbitrary b, Eq b, Storable b ) =>
                       [Int16]
                    -> [Double]
                    -> Bool
prop_roundtripTuple xs ys = (Right (xsv, ysv)) == decode (encode (xsv, ysv))
  where (xsv, ysv) = (fromList xs, fromList ys)