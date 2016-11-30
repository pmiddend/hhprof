module Main where

import Prelude(fromIntegral,(-),undefined,error)
import Data.List(intercalate)
import Data.String(String)
import Data.Either(Either(..))
import System.IO(IO,putStrLn)
import System.Environment(getArgs)
import Data.Eq((/=))
import Data.Function(($))
import Data.Monoid((<>))
import qualified Data.ByteString as BS
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Attoparsec.ByteString(Parser,takeWhile,manyTill,word8,count,take,parseOnly,anyWord8,many')
import Data.Attoparsec.Binary(anyWord32be,anyWord64be,anyWord16be)
import Control.Monad(return,(>>=))
import Control.Applicative((<*>))
import Data.Functor((<$>))
import Data.Bits((.|.),shift)
import Text.Show(Show,show)

data Header = Header {
    intro :: BS.ByteString
  , identifierSize :: Word32
  , baseDate :: Word64
} deriving(Show)

headerParser :: Parser Header
headerParser = do
  intro <- takeWhile (/= 0)
  anyWord8
  identifierSize <- anyWord32be
  let to64 = (fromIntegral :: Word32 -> Word64)
  dateHigh <- to64 <$> anyWord32be
  dateLow <- to64 <$> anyWord32be
  return $ Header intro identifierSize (shift dateHigh 32 .|. dateLow)

siteParser :: Parser AllocSite
siteParser = AllocSite <$> anyWord8 <*> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be

traceParser :: Parser Trace
traceParser = Trace <$> anyWord32be <*> anyWord32be

tagParser :: Word32 -> Parser a -> Parser (Tag a)
tagParser idLength idParser = do
  tagType <- anyWord8
  tagTime <- anyWord32be
  length <- anyWord32be
  case tagType of
    0x01 -> do
      thisId <- idParser
      rest <- take (fromIntegral (length - idLength))
      return (TagUtf8String thisId rest)
    0x02 -> TagLoadClass <$> anyWord32be <*> idParser <*> anyWord32be <*> idParser
    0x03 -> TagUnloadClass <$> anyWord32be
    0x04 -> TagStackFrame <$> idParser <*> idParser <*> idParser <*> idParser <*> anyWord32be <*> anyWord32be
    0x05 -> do
      stackTraceSerialNumber <- anyWord32be
      threadSerialNumber <- anyWord32be
      numberOfFrames <- anyWord32be
      frameIds <- count (fromIntegral numberOfFrames) idParser
      return $ TagStackTrace stackTraceSerialNumber threadSerialNumber numberOfFrames frameIds
    0x06 -> do
      bitMaskFlags <- anyWord16be
      cutoffRatio <- anyWord32be
      liveBytes <- anyWord32be
      liveInstances <- anyWord32be
      bytesAllocated <- anyWord64be
      instancesAllocated <- anyWord64be
      siteCount <- anyWord32be
      sites <- count (fromIntegral siteCount) siteParser
      return $ TagAllocSites bitMaskFlags cutoffRatio liveBytes liveInstances bytesAllocated instancesAllocated siteCount sites
    0x07 -> TagHeapSummary <$> anyWord32be <*> anyWord32be <*> anyWord64be <*> anyWord64be
    0x0A -> TagStartThread <$> anyWord32be <*> idParser <*> anyWord32be <*> idParser<*> idParser<*> idParser
    0x0B -> TagEndThread <$> anyWord32be
    0x2C -> return TagHeapDumpEnd
    0x0D -> do
      numberSamples <- anyWord32be
      tracesCount <- anyWord32be
      traces <- count (fromIntegral tracesCount) traceParser
      return $ TagCpuSamples numberSamples tracesCount traces
    0x0E -> TagControlSettings <$> anyWord32be <*> anyWord16be
    0x0C -> do
      hd <- parseSubTag idParser
      return $ TagHeapDump [hd]
    0x1C -> parseHeapDump idParser
    _ -> do
      _ <- take (fromIntegral length)
      return TagDummy

parseHeapDump :: Parser a -> Parser (Tag a)
parseHeapDump idParser = TagHeapDump <$> manyTill (parseSubTag idParser) (word8 0x2C)

parseSubTag :: Parser a -> Parser (SubTag a)
parseSubTag idParser = do
  tagType <- anyWord8
  case tagType of
    0xFF -> SubTagRootUnknown <$> idParser
    0x01 -> SubTagRootJniGlobal <$> idParser <*> idParser
    0x02 -> SubTagRootJniLocal <$> idParser <*> anyWord32be <*> anyWord32be
    0x03 -> SubTagRootJavaFrame <$> idParser <*> anyWord32be <*> anyWord32be
    0x04 -> SubTagRootNativeStack <$> idParser <*> anyWord32be
    0x05 -> SubTagRootStickyClass <$> idParser
    0x06 -> SubTagRootThreadBlock <$> idParser <*> anyWord32be
    0x07 -> SubTagRootMonitorUsed <$> idParser
    0x08 -> SubTagRootThreadObject <$> idParser <*> anyWord32be <*> anyWord32be
    0x20 -> do
      a0 <- idParser
      a1 <- anyWord32be
      a2 <- idParser
      a3 <- idParser
      a4 <- idParser
      a5 <- idParser
      a6 <- idParser
      a7 <- idParser
      a8 <- anyWord32be
      constantPoolCount <- anyWord16be
      constantPool <- count (fromIntegral constantPoolCount) (constantPoolParser idParser)
      staticFieldCount <- anyWord16be
      staticFields <- count (fromIntegral staticFieldCount) (staticFieldParser idParser)
      instanceFieldCount <- anyWord16be
      instanceFields <- count (fromIntegral instanceFieldCount) (instanceFieldParser idParser)
      return $ SubTagClassDump a0 a1 a2 a3 a4 a5 a6 a7 a8 constantPool staticFields instanceFields
    0x21 -> do
      a0 <- idParser
      a1 <- anyWord32be
      a2 <- idParser
      byteCount <- anyWord32be
      bytes <- take (fromIntegral byteCount)
      return $ SubTagInstanceDump a0 a1 a2 byteCount bytes
    0x22 -> do
      arrayObjectId <- idParser
      stackTraceSerialNumber <- anyWord32be
      numberOfElements <- anyWord32be
      arrayClassObjectId <- idParser
      elements <- count (fromIntegral numberOfElements) idParser
      return $ SubTagObjectArrayDump arrayObjectId stackTraceSerialNumber numberOfElements arrayClassObjectId elements
    0x23 -> do
      arrayObjectId <- idParser
      stackTraceSerialNumber <- anyWord32be
      numberOfElements <- anyWord32be
      elementType <- anyWord8
      elements <- take (fromIntegral numberOfElements)
      return $ SubTagPrimitiveArrayDump arrayObjectId stackTraceSerialNumber numberOfElements elementType elements
    _ -> return SubTagDummy

constantPoolParser :: Parser a -> Parser (ConstantPoolRecord a)
constantPoolParser idParser = do
  index <- anyWord16be
  entryType <- anyWord8
  ConstantPoolRecord index <$> recordFieldParser idParser entryType

instanceFieldParser :: Parser a -> Parser (InstanceField a)
instanceFieldParser idParser = InstanceField <$> idParser <*> anyWord8

staticFieldParser :: Parser a -> Parser (StaticField a)
staticFieldParser idParser = do
  id <- idParser
  fieldType <- anyWord8
  StaticField id <$> recordFieldParser idParser fieldType

data Tag a = TagUtf8String a BS.ByteString
           | TagLoadClass Word32 a Word32 a
           | TagUnloadClass Word32
           | TagStackFrame a a a a Word32 Word32
           | TagStackTrace Word32 Word32 Word32 [a]
           | TagAllocSites Word16 Word32 Word32 Word32 Word64 Word64 Word32 [AllocSite]
           | TagHeapSummary Word32 Word32 Word64 Word64
           | TagStartThread Word32 a Word32 a a a
           | TagEndThread Word32
           | TagHeapDump [SubTag a]
           | TagHeapDumpEnd
           | TagCpuSamples Word32 Word32 [Trace]
           | TagControlSettings Word32 Word16
           | TagDummy deriving(Show)

data SubTag a = SubTagRootUnknown a
              | SubTagRootJniGlobal a a
              | SubTagRootJniLocal a Word32 Word32
              | SubTagRootJavaFrame a Word32 Word32
              | SubTagRootNativeStack a Word32
              | SubTagRootStickyClass a
              | SubTagRootThreadBlock a Word32
              | SubTagRootMonitorUsed a
              | SubTagRootThreadObject a Word32 Word32
              | SubTagClassDump a Word32 a a a a a a Word32 [ConstantPoolRecord a] [StaticField a] [InstanceField a]
              | SubTagInstanceDump a Word32 a Word32 BS.ByteString
              | SubTagObjectArrayDump a Word32 Word32 a [a]
              | SubTagPrimitiveArrayDump a Word32 Word32 Word8 BS.ByteString
              | SubTagDummy
              deriving(Show)

data ConstantPoolRecord a = ConstantPoolRecord {
  index :: Word16,
  field :: RecordField a
  } deriving(Show)

data RecordField a =
    RecordField8 Word8
  | RecordField16 Word16
  | RecordField32 Word32
  | RecordField64 Word64
  | RecordFieldId a
  deriving(Show)

recordFieldParser :: Parser a -> Word8 -> Parser (RecordField a)
recordFieldParser idParser fieldType =
  case fieldType of
    2 -> RecordFieldId <$> idParser
    4 -> RecordField8 <$> anyWord8 -- boolean, 8 bit is speculative!
    5 -> RecordField16 <$> anyWord16be -- char, should be 16 bit
    6 -> RecordField32 <$> anyWord32be -- float
    7 -> RecordField64 <$> anyWord64be -- double
    8 -> RecordField8 <$> anyWord8 -- byte
    9 -> RecordField16 <$> anyWord16be -- short
    10 -> RecordField32 <$> anyWord32be -- int
    11 -> RecordField64 <$> anyWord64be -- long
    x -> error $ "invalid record field type " <> show x
                                                  

data StaticField a = StaticField { staticFieldId :: a, staticField :: RecordField a } deriving(Show)

data InstanceField a = InstanceField { instanceFieldId :: a, instanceFieldType :: Word8 } deriving(Show)

data Trace = Trace Word32 Word32 deriving(Show)

data AllocSite = AllocSite Word8 Word32 Word32 Word32 Word32 Word32 Word32 deriving(Show)

data Body = Body32 [Tag Word32]
          | Body64 [Tag Word64]
          deriving(Show)

parseBody :: Header -> Parser Body
parseBody header =
  case identifierSize header of
    4 -> Body32 <$> (many' (tagParser 4 anyWord32be :: Parser (Tag Word32)))
    8 -> Body64 <$> (many' (tagParser 8 anyWord64be :: Parser (Tag Word64)))

data HProfFile = HProfFile Header Body deriving(Show)

hProfFileParser :: Parser HProfFile
hProfFileParser = do
  header <- headerParser
  body <- parseBody header
  return $ HProfFile header body

hProfFileToString :: HProfFile -> String
hProfFileToString (HProfFile header body) = show header <> "\n" <> showBody body
  where showBody (Body32 tags) = intercalate "\n" (show <$> tags)
        showBody (Body64 tags) = intercalate "\n" (show <$> tags)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      putStrLn $ "parsing " <> x <> "\n"
      fileContents <- BS.readFile x
      case parseOnly hProfFileParser fileContents of
        Left e -> putStrLn $ "error parsing: " <> e
        Right result -> putStrLn $ "success:\n" <> hProfFileToString result
    _ ->
      putStrLn $ "invalid input"
