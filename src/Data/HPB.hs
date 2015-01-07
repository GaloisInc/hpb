{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HPB (
    -- * Interface for message.
    FieldNumber
  , MessageRep
  , HasMessageRep(..)
  , emptyMessageRep
  , Required(..)
  , PackedStatus(..)
  , FieldDef
  , serializeMessage
  , serializeMessage'
  , deserializeMessage
  , deserializeMessage'
  , readDelimitedFromHandle
    -- * Field definitions
  , int32Field
  , int64Field
  , uint32Field
  , uint64Field
  , sint32Field
  , sint64Field
  , boolField
  , enumField
    -- ** Length delimited fields
  , bytesField
  , stringField
  , messageField
    -- ** Fixed width fields
  , fixed32Field
  , fixed64Field
  , sfixed32Field
  , sfixed64Field
  , floatField
  , doubleField
    -- * Repeated field definitions
  , int32RepeatedField
  , int64RepeatedField
  , uint32RepeatedField
  , uint64RepeatedField
  , sint32RepeatedField
  , sint64RepeatedField
  , boolRepeatedField
  , enumRepeatedField
    -- ** Length delimited fields
  , bytesRepeatedField
  , stringRepeatedField
  , messageRepeatedField
    -- ** Fixed width fields
  , fixed32RepeatedField
  , fixed64RepeatedField
  , sfixed32RepeatedField
  , sfixed64RepeatedField
  , floatRepeatedField
  , doubleRepeatedField
    -- * Re-exports
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.Word.Word32
  , Data.Word.Word64
  , Seq.Seq
  , Data.Text.Text
  , B.ByteString

  , Data.Monoid.Monoid(..)
  , (Data.Monoid.<>)
  , Data.String.fromString

  , (Control.Lens.&)
  , Control.Lens.Simple
  , Control.Lens.Lens
  , Control.Lens.lens

  , Prelude.Bool(..)
  , Prelude.Eq
  , Prelude.Enum(..)
  , Prelude.Ord
  , Prelude.Show
  , Prelude.error
  , Prelude.show
  , Prelude.undefined
  , (Prelude.++)
  , (Prelude..)
  ) where

import Control.Applicative
import Control.Lens hiding (Getter, Setter)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.HashTable.ST.Basic as H
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Sequence as Seq
import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word
import Foreign
import System.IO

------------------------------------------------------------------------
-- WireType

type WireType = Word32

varintType :: WireType
varintType = 0

fixed64Type :: WireType
fixed64Type = 1

lengthDelimType :: WireType
lengthDelimType = 2

fixed32Type :: WireType
fixed32Type = 5

------------------------------------------------------------------------
-- Zigzag primitives

zigzag32 :: Int32 -> Word32
zigzag32 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 31)

izigzag32 :: Word32 -> Int32
izigzag32 x = fromIntegral $ (x `shiftR` 1) `xor` mask
  where mask = negate (x .&. 0x1)

zigzag64 :: Int64 -> Word64
zigzag64 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 63)

izigzag64 :: Word64 -> Int64
izigzag64 x = fromIntegral $ (x `shiftR` 1) `xor` mask
  where mask = negate (x .&. 0x1)

------------------------------------------------------------------------
-- Word bytesting primitives.

shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w32 = shiftL

shiftl_w64 :: Word64 -> Int -> Word64
shiftl_w64 = shiftL

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

------------------------------------------------------------------------
-- FieldNumber

-- | Identifier for a message field.
type FieldNumber = Word32

type ReadFn a = B.ByteString -> (a, B.ByteString)

readVarint :: ReadFn Word64
readVarint = runState (readVarint' nextByteInByteString)

nextByteInByteString :: State B.ByteString Word8
nextByteInByteString = state f
  where f s0 =
          case B.uncons s0 of
            Nothing -> error "Expected varint before end of buffer."
            Just (w, s) -> (w, s)

{-# INLINE readVarint' #-}
readVarint' :: Monad m => m Word8 -> m Word64
readVarint' = go 0 0
  where go sh old f = do
          w <- f
          let new_val = old .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
          if (w .&. 0x80) /= 0 then
            go (sh + 7) new_val f
          else
            return new_val

readUntilEnd :: ReadFn a
             -> B.ByteString
             -> [a]
readUntilEnd r s
  | B.null s = []
  | otherwise =
     let (n,s') = r s
      in n : readUntilEnd r s'

------------------------------------------------------------------------
-- Serialization primitives

varint32 :: Word32 -> Builder
varint32 w | low7 == w = word8 (fromIntegral low7)
           | otherwise = word8 (fromIntegral (0x80 .|. low7))
                      <> varint32 (w `shiftR` 7)
  where low7 = w .&. 0x7F

varint64 :: Word64 -> Builder
varint64 w | low7 == w = word8 (fromIntegral low7)
           | otherwise = word8 (fromIntegral (0x80 .|. low7))
                      <> varint64 (w `shiftR` 7)
  where low7 = w .&. 0x7F

fieldNum :: FieldNumber -> WireType -> Builder
fieldNum num tp = varint32 (num `shiftL` 3 .|. tp)

------------------------------------------------------------------------
-- MessageRep

data Required a
   = Req
   | Opt a

data MessageRep a
   = Rep { initMessage :: !a
         , mergeMessage :: !(a -> a -> a)
         , messageName :: !Text
         , fieldDeserializers :: !(HMap.HashMap FieldNumber (FieldDeserializer a))
         , fieldSerializers :: !(V.Vector (a -> Builder))
         , requiredFields :: !(V.Vector (FieldNumber, Text))
         }

insSerializer :: Simple Lens a f
              -> (f -> Builder)
              -> MessageRep a
              -> MessageRep a
insSerializer l serializer rep =
  rep { fieldSerializers = V.snoc (fieldSerializers rep) (serializer . view l) }

insDeserializer :: FieldNumber -> FieldDeserializer a -> MessageRep a -> MessageRep a
insDeserializer num fd rep =
  rep { fieldDeserializers = HMap.insert num fd (fieldDeserializers rep) }

insRequired :: FieldNumber -> Text -> MessageRep a -> MessageRep a
insRequired num nm rep =
  rep { requiredFields = V.snoc (requiredFields rep) (num, nm) }

type SeenFields s = H.HashTable s FieldNumber ()

markSeen :: SeenFields s -> FieldNumber -> ST s ()
markSeen seen n = H.insert seen n ()

checkSeen :: SeenFields s -> MessageRep a -> ST s ()
checkSeen seenFields rep = do
  V.forM_ (requiredFields rep) $ \(num,nm) -> do
    mr <- H.lookup seenFields num
    when (isNothing mr) $ do
      fail $ "Missing required field " ++ Text.unpack nm
          ++ " when deserializing a "
          ++ Text.unpack (messageName rep) ++ " message."

type GetST s = StateT B.ByteString (ST s)

newtype FieldDeserializer a
   = FD { unFD :: forall s
                . SeenFields s
               -> WireType
               -> a
               -> GetST s a
        }

deserializeMessage :: HasMessageRep a => B.ByteString -> a
deserializeMessage = deserializeMessage' messageRep

deserializeMessage' :: MessageRep a -> B.ByteString -> a
deserializeMessage' rep s = runST $ do
  seenFields <- H.new
  evalStateT (populateMessage seenFields rep (initMessage rep)) s

readDelimitedFromHandle :: HasMessageRep a => Handle -> IO a
readDelimitedFromHandle h = do
  len <- alloca $ \p -> do
    readVarint' $ do
      read_cnt <- hGetBuf h p 1
      when (read_cnt /= 1) $ fail "Could not read next byte."
      peek p
  bs <- B.hGet h (fromIntegral len)
  return $ deserializeMessage bs

readFixedField :: Int -> ReadFn B.ByteString
readFixedField n s
  | B.length s < n = error "Unexpected end of buffer."
  | otherwise = B.splitAt n s

readLengthDelimField :: GetST s B.ByteString
readLengthDelimField = do
  len <- doRead readVarint
  doRead $ readFixedField (fromIntegral len)

doRead :: MonadState B.ByteString m => (B.ByteString -> (a, B.ByteString)) -> m a
doRead f = do
  s <- get
  let (r, s') = f s
  put s'
  return r

populateMessage :: SeenFields s
                -> MessageRep a
                -> a
                -> GetST s a
populateMessage seenFields rep msg = do
  isEmpty <- gets B.null
  if isEmpty then do
    lift $ checkSeen seenFields rep
    return msg
  else do
    idx <- doRead readVarint
    when (idx >= 2^(32::Int)) $ do
      fail $ "Field index is out of range."
    let num = fromIntegral (idx `shiftR` 3)
        tp = fromIntegral $ idx .&. 0x7
    case HMap.lookup num (fieldDeserializers rep) of
      Just fd -> do
        msg' <- unFD fd seenFields tp msg
        populateMessage seenFields rep msg'
      Nothing -> do
        -- Skip message
        case tp of
          -- varint type
          0 -> do
            _ <- doRead readVarint
            populateMessage seenFields rep msg
          -- fixed64 type.
          1 -> do
            _ <- doRead $ readFixedField 8
            populateMessage seenFields rep msg
          -- length delim type
          2 -> do
            _ <- readLengthDelimField
            populateMessage seenFields rep msg
          -- fixed32 type
          5 -> do
            _ <- doRead $ readFixedField 4
            populateMessage seenFields rep msg
          _ -> fail $ "Unsupported field type " ++ show tp ++ " before end of message."

serializeMessage :: HasMessageRep a => a -> Builder
serializeMessage = serializeMessage' messageRep

serializeMessage' :: MessageRep a -> a -> Builder
serializeMessage' rep x = V.foldr (\f s -> f x <> s) mempty (fieldSerializers rep)

emptyMessageRep :: Monoid a => Text -> MessageRep a
emptyMessageRep nm =
  Rep { initMessage = mempty
      , mergeMessage = mappend
      , messageName = nm
      , fieldSerializers = V.empty
      , fieldDeserializers = HMap.empty
      , requiredFields = V.empty
      }

type InsertFn a f = f -> a -> a

setTo :: Simple Lens a f -> InsertFn a f
setTo l = (l .~)

appendTo :: Simple Lens a (Seq f) -> InsertFn a f
appendTo l v = l %~ (Seq.|> v)

maybeApplyTo :: InsertFn a f
             -> InsertFn a (Maybe f)
maybeApplyTo g = \mv ->
  case mv of
    Nothing -> id
    Just v -> g v

-- | This is the type for functions used to add fields to the
-- message representation.
type FieldDef a f
   = FieldNumber
   -> Simple Lens a f
   -> MessageRep a
   -> MessageRep a

unexpectedType :: Monad m => Text -> Text -> m a
unexpectedType mnm nm = do
  fail $ "Unexpected type when decoding field " ++ Text.unpack nm
    ++ " in " ++ Text.unpack mnm

serializeOpt :: Eq v => v -> (v -> Builder) -> v -> Builder
serializeOpt d f v | v == d = mempty
                   | otherwise = f v

encodeField :: FieldNumber -> WireType -> Builder -> Builder
encodeField num tp v = fieldNum num tp <> v

-- | Function for reading a varint field.
{-# INLINE varintField #-}
varintField :: Eq f
            => (f -> Builder) -- ^ Projection function for mapping from field to encoding.
            -> (Word64 -> Maybe f)
               -- ^ Injection function for mapping from varint to field value.
            -> Text
            -> Required f
            -> FieldDef a f
varintField proj inj = \nm req num fieldLens rep -> do
  case req of
    Req -> rep & insSerializer fieldLens (encodeField num varintType . proj)
               & insDeserializer num (FD deserial)
               & insRequired num nm
      where mnm = messageName rep
            deserial seen tp msg
              | tp == varintType = do
                lift $ markSeen seen num
                w <- doRead readVarint
                return $ msg & maybeApplyTo (setTo fieldLens) (inj w)
              | otherwise = do
                unexpectedType mnm nm
    Opt d -> rep & insSerializer fieldLens serial
                 & insDeserializer num (FD deserial)
      where mnm = messageName rep
            serial = serializeOpt d (encodeField num varintType . proj)
            deserial _ tp msg
              | tp == varintType = do
                w <- doRead readVarint
                return $ msg & maybeApplyTo (setTo fieldLens) (inj w)
              | tp == lengthDelimType = do
                intValues <- readLengthDelimField
                let mv = lastOf folded (mapMaybe inj (readUntilEnd readVarint intValues))
                return $ msg & maybeApplyTo (setTo fieldLens) mv
              | otherwise = do
                unexpectedType mnm nm

-- | Update field with last varint in field.
updateLastFixed :: Int
                -> (B.ByteString -> f)
                -> InsertFn a f
                -> InsertFn a B.ByteString
updateLastFixed n inj setter a x
    | l `rem` n /= 0 = error "Fixed field has unexpected length."
    | B.null a  = x
    | otherwise = setter (inj end) x
  where l = B.length a
        end = B.drop (l - n) a

-- | Function for reading a varint field.
{-# INLINE fixedField #-}
fixedField :: Eq f
           => WireType
           -> Int
           -> (f -> Builder)
           -> (B.ByteString -> f)
           -> Text
           -> Required f
           -> FieldDef a f
fixedField wireType n proj inj nm req num fieldLens rep = do
  let encode = encodeField num wireType . proj
  case req of
    Req -> rep & insSerializer fieldLens encode
               & insDeserializer num (FD deserial)
               & insRequired num nm
      where mnm = messageName rep
            deserial seen tp msg
              | tp == wireType = do
                lift $ markSeen seen num
                wb <- doRead $ readFixedField n
                return $ msg & fieldLens .~ inj wb
              | otherwise = do
                unexpectedType mnm nm
    Opt d -> rep & insSerializer fieldLens (serializeOpt d encode)
                 & insDeserializer num (FD deserial)
      where mnm = messageName rep
            deserial _ tp msg
              | tp == wireType = do
                wb <- doRead $ readFixedField n
                return $ msg & fieldLens .~ inj wb
              | tp == lengthDelimType = do
                f <- readLengthDelimField
                return $ updateLastFixed n inj (fieldLens .~) f msg
              | otherwise = do
                unexpectedType mnm nm

encodeStrictLengthDelimField :: FieldNumber -> B.ByteString -> Builder
encodeStrictLengthDelimField num b =
  encodeField num lengthDelimType $
     varint64 (fromIntegral (B.length b)) <> byteString b

encodeLengthDelimField :: FieldNumber -> Builder -> Builder
encodeLengthDelimField num b = do
    encodeField num lengthDelimType $
      varint64 (fromIntegral (LazyB.length s)) <> lazyByteString s
  where s = toLazyByteString b


recordFieldNum :: FieldNumber
               -> FieldDeserializer a
               -> FieldDeserializer a
recordFieldNum num (FD d) = FD deserial
  where deserial seen tp msg = do
          lift $ markSeen seen num
          d seen tp msg

deserializeLengthDelimField :: MessageRep a
                            -> Text
                            -> InsertFn a B.ByteString
                            -> FieldDeserializer a
deserializeLengthDelimField rep nm setter = FD deserial
  where mnm = messageName rep
        deserial _ tp msg
          | tp == lengthDelimType = do
            w <- readLengthDelimField
            return $ msg & setter w
          | otherwise = do
            unexpectedType mnm nm

-- | Function for reading a varint field.
{-# INLINE lengthDelimField #-}
lengthDelimField :: Eq f
                 => (f -> B.ByteString)
                    -- ^ Projection function for mapping from field to encoding.
                 -> (B.ByteString -> f)
                    -- ^ Injection function for mapping from data to field value.
                 -> Text
                 -> Required f
                 -> FieldDef a f
lengthDelimField proj inj nm req num fieldLens rep = do
  let encode = encodeStrictLengthDelimField num . proj
  let decode = deserializeLengthDelimField rep nm (setTo fieldLens . inj)
  case req of
    Req -> rep & insSerializer fieldLens encode
               & insDeserializer num (recordFieldNum num decode)
               & insRequired num nm
    Opt d -> rep & insSerializer fieldLens (serializeOpt d encode)
                 & insDeserializer num decode

int32Field :: Text -> Required Int32 -> FieldDef a Int32
int32Field = varintField (varint32 . fromIntegral) (Just . fromIntegral)
{-# INLINE int32Field #-}

int64Field :: Text -> Required Int64 -> FieldDef a Int64
int64Field = varintField (varint64 . fromIntegral) (Just . fromIntegral)
{-# INLINE int64Field #-}

uint32Field :: Text -> Required Word32 -> FieldDef a Word32
uint32Field = varintField varint32 (Just . fromIntegral)
{-# INLINE uint32Field #-}

uint64Field :: Text -> Required Word64 -> FieldDef a Word64
uint64Field = varintField varint64 Just
{-# INLINE uint64Field #-}

sint32Field :: Text -> Required Int32 -> FieldDef a Int32
sint32Field = varintField (varint32 . zigzag32) (Just . izigzag32 . fromIntegral)
{-# INLINE sint32Field #-}

sint64Field :: Text -> Required Int64 -> FieldDef a Int64
sint64Field = varintField (varint64 . zigzag64) (Just . izigzag64)
{-# INLINE sint64Field #-}

boolField :: Text -> Required Bool -> FieldDef a Bool
boolField = varintField (word8 . fromIntegral . fromEnum) (Just . (/= 0))

enumSetter :: Enum x => [x] -> Word64 -> Maybe x
enumSetter legal = setter
  where legalSet = HSet.fromList $ fmap (fromIntegral.fromEnum) legal
        setter w | HSet.member w legalSet = Just (toEnum (fromIntegral w))
                 | otherwise = Nothing

enumField :: (Enum x, Eq x)
          => Text
          -> Required x -- ^ Default value for type.
          -> [x] -- ^ List of all legal enum values.
          -> FieldDef a x
enumField nm req legal =
  varintField (varint32 . fromIntegral . fromEnum) (enumSetter legal) nm req

fixed32Field :: Text -> Required Word32 -> FieldDef a Word32
fixed32Field = fixedField fixed32Type 4 word32LE word32le

fixed64Field :: Text -> Required Word64 -> FieldDef a Word64
fixed64Field = fixedField fixed64Type 8 word64LE word64le

sfixed32Field :: Text -> Required Int32 -> FieldDef a Int32
sfixed32Field = fixedField fixed32Type 4 int32LE (fromIntegral . word32le)

sfixed64Field :: Text -> Required Int64 -> FieldDef a Int64
sfixed64Field = fixedField fixed64Type 8 int64LE (fromIntegral . word64le)

floatField :: Text -> Required Float -> FieldDef a Float
floatField  = fixedField fixed32Type 4 floatLE  (wordToFloat  . word32le)

doubleField :: Text -> Required Double -> FieldDef a Double
doubleField = fixedField fixed64Type 8 doubleLE (wordToDouble . word64le)

bytesField :: Text -> Required B.ByteString -> FieldDef a B.ByteString
bytesField = lengthDelimField id id

stringField :: Text -> Required Text -> FieldDef a Text
stringField = lengthDelimField Text.encodeUtf8 Text.decodeUtf8

messageField :: Monoid m
             => MessageRep m
             -> Text
             -> Bool -- ^ Indicates if the message is required.
             -> FieldDef a m
messageField field_rep nm req num fieldLens rep =
    rep & insSerializer fieldLens serial
        & insDeserializer num (if req then recordFieldNum num deserial else deserial)
        & (if req then insRequired num nm else id)
  where serial = encodeLengthDelimField num
               . serializeMessage' field_rep
        deserial = deserializeLengthDelimField rep nm
                     (setTo fieldLens . deserializeMessage' field_rep)

-- | This is the type for functions used to add fields to the
-- message representation.
type RepeatedFieldDef a f
   = FieldNumber
   -> Simple Lens a (Seq f)
   -> MessageRep a
   -> MessageRep a

-- | Indicates if repeated field may be packed.
data PackedStatus = Packed | Unpacked

-- | Serialize a field that may be packed.
serializePackedField :: FieldNumber
                     -> WireType
                        -- ^ Wire type for individual values.
                     -> Seq Builder
                     -> Builder
serializePackedField num tp elts
    | len == 0 = mempty
    | len == 1 = fieldNum num tp <> (elts `Seq.index` 0)
    | otherwise = encodeLengthDelimField num $ Fold.fold elts
  where -- Get number of elements
        len = Seq.length elts

serializeRepeatedField :: PackedStatus
                       -> (f -> Builder)
                       -> WireType
                       -> FieldNumber
                       -> Seq f
                       -> Builder
serializeRepeatedField packed proj tp num elts =
  case packed of
    Packed   -> serializePackedField tp num (proj <$> elts)
    Unpacked -> Fold.fold ((\v -> fieldNum num tp <> proj v) <$> elts)

-- | Function for reading a varint field.
{-# INLINE varintRepeatedField #-}
varintRepeatedField :: forall a f
                     . Eq f
                    => (f -> Builder) -- ^ Projection function for mapping from field to encoding.
                    -> (Word64 -> Maybe f)
                       -- ^ Injection function for mapping from varint to field value.
                    -> Text
                    -> PackedStatus
                    -> RepeatedFieldDef a f
varintRepeatedField proj inj nm packed num fieldLens rep =
    rep & insSerializer fieldLens serial
        & insDeserializer num (FD deserial)
  where mnm = messageName rep
        serial = serializeRepeatedField packed proj varintType num
        deserial _ tp msg
          | tp == varintType = do
            w <- doRead readVarint
            return $ msg & maybeApplyTo (appendTo fieldLens) (inj w)
          | tp == lengthDelimType = do
            s0 <- readLengthDelimField
            let intValues = mapMaybe inj $ readUntilEnd readVarint s0
            return $ msg & fieldLens %~ (Seq.>< Seq.fromList intValues)
          | otherwise = do
            unexpectedType mnm nm

-- | Function for reading a varint field.
{-# INLINE fixedRepeatedField #-}
fixedRepeatedField :: Eq f
                   => WireType
                   -> Int
                   -> (f -> Builder)
                   -> (B.ByteString -> f)
                   -> Text
                   -> PackedStatus
                   -> RepeatedFieldDef a f
fixedRepeatedField wireType n proj inj nm packed num fieldLens rep =
    rep & insSerializer fieldLens serial
        & insDeserializer num (FD deserial)
  where mnm = messageName rep
        serial = serializeRepeatedField packed proj wireType num
        deserial _ tp msg
          | tp == wireType = do
            wb <- doRead $ readFixedField n
            return $ msg & appendTo fieldLens (inj wb)
          | tp == lengthDelimType = do
            s0 <- readLengthDelimField
            let intValues = inj <$> readUntilEnd (readFixedField n) s0
            return $ msg & fieldLens %~ (Seq.>< Seq.fromList intValues)
          | otherwise = do
            unexpectedType mnm nm

-- | Function for reading a varint field.
{-# INLINE lengthDelimRepeatedField #-}
lengthDelimRepeatedField :: Eq f
                         => (f -> B.ByteString)
                            -- ^ Projection function for mapping from field to encoding.
                         -> (B.ByteString -> f)
                            -- ^ Injection function for mapping from data to field value.
                         -> Text
                         -> RepeatedFieldDef a f
lengthDelimRepeatedField proj inj nm num fieldLens rep =
    rep & insSerializer fieldLens serial
        & insDeserializer num (FD deserial)
  where mnm = messageName rep
        serial = Fold.fold . fmap (encodeStrictLengthDelimField num . proj)
        deserial _ tp msg
          | tp == lengthDelimType = do
            w <- readLengthDelimField
            return $ msg & fieldLens %~ (Seq.|> inj w)
          | otherwise = do
            unexpectedType mnm nm

int32RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Int32
int32RepeatedField = varintRepeatedField (varint32 . fromIntegral) (Just . fromIntegral)

int64RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Int64
int64RepeatedField = varintRepeatedField (varint64 . fromIntegral) (Just . fromIntegral)

uint32RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Word32
uint32RepeatedField = varintRepeatedField varint32 (Just . fromIntegral)

uint64RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Word64
uint64RepeatedField = varintRepeatedField varint64 Just

sint32RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Int32
sint32RepeatedField =
  varintRepeatedField (varint32 . zigzag32) (Just . izigzag32 . fromIntegral)

sint64RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Int64
sint64RepeatedField =
  varintRepeatedField (varint64 . zigzag64) (Just . izigzag64)

boolRepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Bool
boolRepeatedField =
  varintRepeatedField (word8 . fromIntegral . fromEnum) (Just . (/= 0))

enumRepeatedField :: (Eq x, Enum x)
                  => Text
                  -> PackedStatus
                  -> [x]
                  -> RepeatedFieldDef a x
enumRepeatedField nm req legal =
  varintRepeatedField (varint32 . fromIntegral . fromEnum) (enumSetter legal) nm req

fixed32RepeatedField :: Text -> PackedStatus -> RepeatedFieldDef a Word32
fixed32RepeatedField = fixedRepeatedField fixed32Type 4 word32LE word32le

fixed64RepeatedField :: Text
                     -> PackedStatus
                     -> RepeatedFieldDef a Word64
fixed64RepeatedField = fixedRepeatedField fixed32Type 8 word64LE word64le

sfixed32RepeatedField :: Text
                      -> PackedStatus
                      -> RepeatedFieldDef a Int32
sfixed32RepeatedField = fixedRepeatedField fixed32Type 4 int32LE (fromIntegral . word32le)

sfixed64RepeatedField :: Text
                      -> PackedStatus
                      -> RepeatedFieldDef a Int64
sfixed64RepeatedField = fixedRepeatedField fixed64Type 8 int64LE (fromIntegral . word64le)

floatRepeatedField :: Text
                   -> PackedStatus
                   -> RepeatedFieldDef a Float
floatRepeatedField = fixedRepeatedField fixed32Type 4 floatLE  (wordToFloat  . word32le)

doubleRepeatedField :: Text
                    -> PackedStatus
                    -> RepeatedFieldDef a Double
doubleRepeatedField = fixedRepeatedField fixed64Type 8 doubleLE (wordToDouble . word64le)

bytesRepeatedField :: Text
                   -> RepeatedFieldDef a B.ByteString
bytesRepeatedField = lengthDelimRepeatedField id id

stringRepeatedField :: Text
                    -> RepeatedFieldDef a Text
stringRepeatedField = lengthDelimRepeatedField Text.encodeUtf8 Text.decodeUtf8

messageRepeatedField :: MessageRep m
                     -> Text
                     -> RepeatedFieldDef a m
messageRepeatedField field_rep nm num fieldLens rep =
    rep & insSerializer fieldLens (Fold.fold . fmap serial)
        & insDeserializer num deserial
  where serial = encodeLengthDelimField num
               . serializeMessage' field_rep
        deserial = deserializeLengthDelimField rep nm
                     (appendTo fieldLens . deserializeMessage' field_rep)

class HasMessageRep tp where
  messageRep :: MessageRep tp