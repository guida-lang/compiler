{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, FlexibleInstances, MagicHash, UnboxedTuples #-}
module Data.Utf8
  ( String
  , Utf8(..)
  , VeryLong
  , Under256
  , isEmpty
  , size
  , contains
  , containsDouble
  , startsWith
  , startsWithChar
  , endsWithWord
  , split
  , join
  , all
  , any
  , toWord16
  , fromWord16
  -- conversions
  , fromChars
  , toChars
  , toBuilder
  , fromPtr
  , fromChunks
  , Chunk(..)
  )
  where


import Prelude hiding (String, all, any, concat)
import Data.Binary (Binary(..), Get, getWord8, putWord8)
import Data.Binary.Put (putBuilder)
import Data.Binary.Get.Internal (readN)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.Char as Char
import qualified Data.List as List
import Data.String (IsString(..))
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import GHC.Exts
  ( Int(I#), Ptr(Ptr), Char(C#)
  , RealWorld
  , ByteArray#, MutableByteArray#
  , isTrue#
  , newByteArray#
  , unsafeFreezeByteArray#
  , sizeofByteArray#
  , copyByteArray#
  , copyAddrToByteArray#
  , copyByteArrayToAddr#
  , writeWord8Array#,
  )
import GHC.IO
import GHC.ST (ST(ST), runST)
import GHC.Prim
import GHC.Word (Word8(W8#), Word16(W16#))



-- UTF-8


data Utf8 size tipe =
  Utf8 ByteArray#


type String = Utf8 VERY_LONG STRING
type VeryLong t = Utf8 VERY_LONG t
type Under256 t = Utf8 UNDER_256 t

data STRING
data VERY_LONG
data UNDER_256



-- IS EMPTY


isEmpty :: Utf8 l t -> Bool
isEmpty (Utf8 ba#) =
  isTrue# (sizeofByteArray# ba# ==# 0#)



-- SIZE


size :: Utf8 l t -> Int
size (Utf8 ba#) =
  I# (sizeofByteArray# ba#)



-- CONTAINS


contains :: Word8 -> Utf8 l t -> Bool
contains (W8# word#) (Utf8 ba#) =
  containsHelp word# ba# 0# (sizeofByteArray# ba#)


containsHelp :: Word# -> ByteArray# -> Int# -> Int# -> Bool
containsHelp word# ba# !offset# len# =
  if isTrue# (offset# <# len#) then
    if isTrue# (eqWord# word# (indexWord8Array# ba# offset#))
      then True
      else containsHelp word# ba# (offset# +# 1#) len#
  else
    False



-- CONTAINS DOUBLE


containsDouble :: Word8 -> Utf8 l t -> Bool
containsDouble (W8# word#) (Utf8 ba#) =
  containsDoubleHelp word# ba# 0# (sizeofByteArray# ba#)


containsDoubleHelp :: Word# -> ByteArray# -> Int# -> Int# -> Bool
containsDoubleHelp word# ba# !offset# len# =
  let !offset1# = offset# +# 1# in
  if isTrue# (offset1# <# len#) then
    if   isTrue# (eqWord# word# (indexWord8Array# ba# offset# ))
      && isTrue# (eqWord# word# (indexWord8Array# ba# offset1#))
    then True
    else containsDoubleHelp word# ba# (offset# +# 1#) len#
  else
    False



-- STARTS WITH


{-# INLINE startsWith #-}
startsWith :: Utf8 l t -> Utf8 l t -> Bool
startsWith (Utf8 ba1#) (Utf8 ba2#) =
  let
    !len1# = sizeofByteArray# ba1#
    !len2# = sizeofByteArray# ba2#
  in
  isTrue# (len1# <=# len2#)
  &&
  isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- STARTS WITH CHAR


startsWithChar :: (Char -> Bool) -> Utf8 l t -> Bool
startsWithChar isGood str@(Utf8 ba#) =
  if isEmpty str then
    False
  else
    let
      !w# = indexWord8Array# ba# 0#
      !char
        | isTrue# (ltWord# w# 0xC0##) = C# (chr# (word2Int# w#))
        | isTrue# (ltWord# w# 0xE0##) = chr2 ba# 0# w#
        | isTrue# (ltWord# w# 0xF0##) = chr3 ba# 0# w#
        | True                        = chr4 ba# 0# w#
    in
    isGood char



-- ENDS WITH WORD


endsWithWord :: Word8 -> Utf8 l t -> Bool
endsWithWord (W8# w#) (Utf8 ba#) =
  let len# = sizeofByteArray# ba# in
  isTrue# (len# ># 0#)
  &&
  isTrue# (eqWord# w# (indexWord8Array# ba# (len# -# 1#)))



-- SPLIT


split :: Word8 -> String -> [String]
split (W8# divider#) str@(Utf8 ba#) =
  splitHelp str 0 (findDividers divider# ba# 0# (sizeofByteArray# ba#) [])


splitHelp :: String -> Int -> [Int] -> [String]
splitHelp str start offsets =
  case offsets of
    [] ->
      [ unsafeSlice str start (size str) ]

    offset : offsets ->
      unsafeSlice str start offset : splitHelp str (offset + 1) offsets


findDividers :: Word# -> ByteArray# -> Int# -> Int# -> [Int] -> [Int]
findDividers divider# ba# !offset# len# revOffsets =
  if isTrue# (offset# <# len#) then
    findDividers divider# ba# (offset# +# 1#) len# $
      if isTrue# (eqWord# divider# (indexWord8Array# ba# offset#))
      then I# offset# : revOffsets
      else revOffsets
  else
    reverse revOffsets


unsafeSlice :: String -> Int -> Int -> String
unsafeSlice str start end =
  let !len = end - start in
  if len == 0 then
    empty
  else
    runST $
    do  mba <- newByteArray len
        copy str start mba 0 len
        freeze mba



-- JOIN


join :: Word8 -> [String] -> String
join sep strings =
  case strings of
    [] ->
      empty

    str:strs ->
      runST $
      do  let !len = List.foldl' (\w s -> w + 1 + size s) (size str) strs
          mba <- newByteArray len
          joinHelp sep mba 0 str strs
          freeze mba


joinHelp :: Word8 -> MBA s -> Int -> String -> [String] -> ST s ()
joinHelp sep mba offset str strings =
  let
    !len = size str
  in
  case strings of
    [] ->
      copy str 0 mba offset len

    s:ss ->
      do  copy str 0 mba offset len
          let !dotOffset = offset + len
          writeWord8 mba dotOffset sep
          let !newOffset = dotOffset + 1
          joinHelp sep mba newOffset s ss



-- ALL


all :: (Char -> Bool) -> Utf8 l t -> Bool
all isGood utf8 =
  not (any (not . isGood) utf8)



-- ANY


any :: (Char -> Bool) -> Utf8 l t -> Bool
any isGood (Utf8 ba#) =
  anyHelp isGood ba# 0# (sizeofByteArray# ba#)


anyHelp :: (Char -> Bool) -> ByteArray# -> Int# -> Int# -> Bool
anyHelp isGood ba# offset# len# =
  if isTrue# (offset# >=# len#) then
    False
  else
    let
      !w# = indexWord8Array# ba# offset#
      !(# char, width# #)
        | isTrue# (ltWord# w# 0xC0##) = (# C# (chr# (word2Int# w#)), 1# #)
        | isTrue# (ltWord# w# 0xE0##) = (# chr2 ba# offset# w#, 2# #)
        | isTrue# (ltWord# w# 0xF0##) = (# chr3 ba# offset# w#, 3# #)
        | True                        = (# chr4 ba# offset# w#, 4# #)
    in
    if isGood char
      then True
      else anyHelp isGood ba# (offset# +# width#) len#



-- EQUAL


instance Eq (Utf8 l t) where
  (==) (Utf8 ba1#) (Utf8 ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
    in
    isTrue# (len1# ==# len2#)
    &&
    isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- COMPARE


instance Ord (Utf8 l t) where
  compare (Utf8 ba1#) (Utf8 ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
      !len#  = if isTrue# (len1# <# len2#) then len1# else len2#
      !cmp#  = compareByteArrays# ba1# 0# ba2# 0# len#
    in
    case () of
      _ | isTrue# (cmp# <# 0#)     -> LT
        | isTrue# (cmp# ># 0#)     -> GT
        | isTrue# (len1# <# len2#) -> LT
        | isTrue# (len1# ># len2#) -> GT
        | True                     -> EQ



-- TO WORD16


toWord16 :: String -> Maybe Word16
toWord16 (Utf8 ba#) =
  let !len# = sizeofByteArray# ba# in
  if isTrue# (len# ==# 0#) then
    Nothing
  else
    let !w# = indexWord8Array# ba# 0# in
    if isTrue# (eqWord# w# 0x30##) then
      if isTrue# (len# ==# 1#) then Just 0 else Nothing
    else
      toWordHelp ba# 0# len# 0##


toWordHelp :: ByteArray# -> Int# -> Int# -> Word# -> Maybe Word16
toWordHelp ba# offset# len# number# =
  if isTrue# (offset# <# len#) then
    let !w# = indexWord8Array# ba# offset# in
    if isTrue# (leWord# w# 0x39##) && isTrue# (geWord# w# 0x30##) then
      let !newNumber# = plusWord# (timesWord# 10## number#) (minusWord# w# 0x30##) in
      toWordHelp ba# (offset# +# 1#) len# newNumber#
    else
      Nothing
  else
    Just (W16# number#)



-- FROM WORD16


fromWord16 :: Word16 -> String
fromWord16 n =
  runST $
    do  let !size = getWordSize n
        mba <- newByteArray size
        writeDigits mba (size - 1) n
        freeze mba


getWordSize :: Word16 -> Int
getWordSize n
  | n < 10  = 1
  | n < 100 = 2
  | True    = ceiling (logBase 10 (fromIntegral n + 1) :: Float)


writeDigits :: MBA s -> Int -> Word16 -> ST s ()
writeDigits !mba !offset !n =
  do  let (q,r) = quotRem n 10
      writeWord8 mba offset (0x30 + fromIntegral r)
      if q <= 0
        then return ()
        else writeDigits mba (offset-1) q



-- FROM STRING


fromChars :: [Char] -> Utf8 l t
fromChars =
  fromString


instance IsString (Utf8 l t) where
  fromString str =
    runST
    (
      do  mba <- newByteArray (sum (map getWidth str))
          writeString mba 0 str
    )


writeString :: MBA s -> Int -> [Char] -> ST s (Utf8 l t)
writeString !mba !offset chars =
  case chars of
    [] ->
      freeze mba

    char : chars
      | n < 0x80 ->
          do  writeWord8 mba (offset    ) (fromIntegral n)
              writeString mba (offset + 1) chars

      | n < 0x800 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 6         ) + 0xC0))
              writeWord8 mba (offset + 1) (fromIntegral ((       n   .&. 0x3F) + 0x80))
              writeString mba (offset + 2) chars

      | n < 0x10000 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 12         ) + 0xE0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeString mba (offset + 3) chars

      | otherwise ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 18         ) + 0xF0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n 12 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 3) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeString mba (offset + 4) chars

      where
        n = Char.ord char


{-# INLINE getWidth #-}
getWidth :: Char -> Int
getWidth char
  | code < 0x80    = 1
  | code < 0x800   = 2
  | code < 0x10000 = 3
  | otherwise      = 4
  where
    code = Char.ord char



-- TO STRING


toChars :: Utf8 l t -> [Char]
toChars (Utf8 ba#) =
  toCharsHelp ba# 0# (sizeofByteArray# ba#)


toCharsHelp :: ByteArray# -> Int# -> Int# -> [Char]
toCharsHelp ba# offset# len# =
  if isTrue# (offset# >=# len#) then
    []
  else
    let
      !w# = indexWord8Array# ba# offset#
      !(# char, width# #)
        | isTrue# (ltWord# w# 0xC0##) = (# C# (chr# (word2Int# w#)), 1# #)
        | isTrue# (ltWord# w# 0xE0##) = (# chr2 ba# offset# w#, 2# #)
        | isTrue# (ltWord# w# 0xF0##) = (# chr3 ba# offset# w#, 3# #)
        | True                        = (# chr4 ba# offset# w#, 4# #)

      !newOffset# = offset# +# width#
    in
    char : toCharsHelp ba# newOffset# len#


{-# INLINE chr2 #-}
chr2 :: ByteArray# -> Int# -> Word# -> Char
chr2 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !c1# = uncheckedIShiftL# (i1# -# 0xC0#) 6#
    !c2# = i2# -# 0x80#
  in
  C# (chr# (c1# +# c2#))


{-# INLINE chr3 #-}
chr3 :: ByteArray# -> Int# -> Word# -> Char
chr3 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !i3# = word2Int# (indexWord8Array# ba# (offset# +# 2#))
    !c1# = uncheckedIShiftL# (i1# -# 0xE0#) 12#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 6#
    !c3# = i3# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3#))


{-# INLINE chr4 #-}
chr4 :: ByteArray# -> Int# -> Word# -> Char
chr4 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !i3# = word2Int# (indexWord8Array# ba# (offset# +# 2#))
    !i4# = word2Int# (indexWord8Array# ba# (offset# +# 3#))
    !c1# = uncheckedIShiftL# (i1# -# 0xF0#) 18#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 12#
    !c3# = uncheckedIShiftL# (i3# -# 0x80#) 6#
    !c4# = i4# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3# +# c4#))



-- TO BUILDER


{-# INLINE toBuilder #-}
toBuilder :: Utf8 l t -> B.Builder
toBuilder =
  \bytes -> B.builder (toBuilderHelp bytes)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Utf8 l t -> B.BuildStep a -> B.BuildStep a
toBuilderHelp !bytes@(Utf8 ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !end !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
        !len = end - offset
      in
      if len <= bLen then
        do  copyToPtr bytes offset bOffset len
            let !br' = B.BufferRange (plusPtr bOffset len) bEnd
            k br'
      else
        do  copyToPtr bytes offset bOffset bLen
            let !offset' = offset + bLen
            return $ B.bufferFull 1 bEnd (go offset' end)



-- FROM PTR


fromPtr :: Ptr Word8 -> Ptr Word8 -> Utf8 l t
fromPtr pos end =
  unsafeDupablePerformIO (stToIO (
    do  let !len = minusPtr end pos
        mba <- newByteArray len
        copyFromPtr pos mba 0 len
        freeze mba
  ))



-- FROM CHUNKS


data Chunk
  = Slice (Ptr Word8) Int
  | Escape Word8
  | CodePoint Int


fromChunks :: [Chunk] -> Utf8 l t
fromChunks chunks =
  unsafeDupablePerformIO (stToIO (
    do  let !len = sum (map chunkToWidth chunks)
        mba <- newByteArray len
        writeChunks mba 0 chunks
        freeze mba
  ))


chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len ->
      len

    Escape _ ->
      2

    CodePoint code ->
      if code < 0xFFFF then 6 else 12


writeChunks :: MBA RealWorld -> Int -> [Chunk] -> ST RealWorld ()
writeChunks mba offset chunks =
  case chunks of
    [] ->
      return ()

    chunk : chunks ->
      case chunk of
        Slice ptr len ->
          do  copyFromPtr ptr mba offset len
              let !newOffset = offset + len
              writeChunks mba newOffset chunks

        Escape word ->
          do  writeWord8 mba offset 0x5C {- \ -}
              writeWord8 mba (offset + 1) word
              let !newOffset = offset + 2
              writeChunks mba newOffset chunks

        CodePoint code ->
          if code < 0xFFFF then
            do  writeCode mba offset code
                let !newOffset = offset + 6
                writeChunks mba newOffset chunks
          else
            do  let (hi,lo) = divMod (code - 0x10000) 0x400
                writeCode mba (offset    ) (hi + 0xD800)
                writeCode mba (offset + 6) (lo + 0xDC00)
                let !newOffset = offset + 12
                writeChunks mba newOffset chunks


-- TODO see if it is faster to writeWord32 a block of hex-as-ascii
writeCode :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeCode mba offset code =
  do  writeWord8 mba offset 0x5C {- \ -}
      writeWord8 mba offset 0x75 {- u -}
      writeHex mba (offset + 2) (shiftR code 12)
      writeHex mba (offset + 3) (shiftR code 8)
      writeHex mba (offset + 4) (shiftR code 4)
      writeHex mba (offset + 5) code


writeHex :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeHex mba !offset !bits =
  do  let !n = fromIntegral bits .&. 0x0F
      writeWord8 mba offset (if n < 10 then 0x30 + n else 0x37 + n)



-- UNDER 256 BINARY


instance Binary (Utf8 UNDER_256 t) where
  put bytes@(Utf8 ba#) =
    do  putWord8 (W8# (int2Word# (sizeofByteArray# ba#)))
        putBuilder (toBuilder bytes)
  get =
    getUnder256 =<< getWord8


{-# INLINE getUnder256 #-}
getUnder256 :: Word8 -> Get (Utf8 UNDER_256 t)
getUnder256 w =
  let !n = fromIntegral w in
  readN n (copyFromByteString n)



-- VERY LONG BINARY


instance Binary (Utf8 VERY_LONG t) where
  put str =
    do  put (size str)
        putBuilder (toBuilder str)
  get =
    getVeryLong =<< get


{-# INLINE getVeryLong #-}
getVeryLong :: Int -> Get (Utf8 VERY_LONG t)
getVeryLong n =
  if n > 0
  then readN n (copyFromByteString n)
  else return empty


{-# NOINLINE empty #-}
empty :: Utf8 l t
empty =
  runST (freeze =<< newByteArray 0)



-- COPY FROM BYTESTRING


{-# INLINE copyFromByteString #-}
copyFromByteString :: Int -> B.ByteString -> Utf8 l t
copyFromByteString len (B.PS fptr offset _) =
  unsafeDupablePerformIO
  (
    do  mba <- stToIO (newByteArray len)
        stToIO (copyFromPtr (unsafeForeignPtrToPtr fptr `plusPtr` offset) mba 0 len)
        touchForeignPtr fptr
        stToIO (freeze mba)
  )



-- PRIMITIVES


data MBA s =
  MBA# (MutableByteArray# s)


newByteArray :: Int -> ST s (MBA s) -- TODO see if newPinnedByteArray for len > 256 is positive
newByteArray (I# len#) =
  ST $ \s ->
    case newByteArray# len# s of
      (# s, mba# #) -> (# s, MBA# mba# #)


freeze :: MBA s -> ST s (Utf8 l t)
freeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, Utf8 ba# #)


copy :: Utf8 l t -> Int -> MBA s -> Int -> Int -> ST s ()
copy (Utf8 ba#) (I# offset#) (MBA# mba#) (I# i#) (I# len#) =
  ST $ \s ->
    case copyByteArray# ba# offset# mba# i# len# s of
      s -> (# s, () #)


copyFromPtr :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyFromPtr (Ptr src#) (MBA# mba#) (I# offset#) (I# len#) =
  ST $ \s ->
    case copyAddrToByteArray# src# mba# offset# len# s of
      s -> (# s, () #)


copyToPtr :: Utf8 l t -> Int -> Ptr a -> Int -> IO ()
copyToPtr (Utf8 ba#) (I# offset#) (Ptr mba#) (I# len#) =
    IO $ \s ->
      case copyByteArrayToAddr# ba# offset# mba# len# s of
        s -> (# s, () #)


{-# INLINE writeWord8 #-}
writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
writeWord8 (MBA# mba#) (I# offset#) (W8# w#) =
  ST $ \s ->
    case writeWord8Array# mba# offset# w# s of
      s -> (# s, () #)