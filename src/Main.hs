module Main where

import Prelude(fromIntegral,(-),undefined)
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
import Data.Attoparsec.ByteString(Parser,takeWhile,count,take,parseOnly,anyWord8,many')
import Data.Attoparsec.Binary(anyWord32be,anyWord64be,anyWord16be)
import Control.Monad(return)
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
    0x0A -> do
      threadSerialNumber <- anyWord32be
      threadObjectId <- idParser
      traceSerialNumber <- anyWord32be
      threadNameStringId <- idParser
      threadGroupNameId <- idParser
      parentGroupNameId <- idParser
      return $ TagStartThread threadSerialNumber threadObjectId traceSerialNumber threadNameStringId threadGroupNameId parentGroupNameId
    0x0B -> TagEndThread <$> anyWord32be
    0x2C -> return TagHeapDumpEnd
    0x0D -> do
      numberSamples <- anyWord32be
      tracesCount <- anyWord32be
      traces <- count (fromIntegral tracesCount) traceParser
      return $ TagCpuSamples numberSamples tracesCount traces
    0x0E -> TagControlSettings <$> anyWord32be <*> anyWord16be
    0x0C -> parseHeapDump idParser
    0x1C -> parseHeapDump idParser
    _ -> do
      _ <- take (fromIntegral length)
      return TagDummy

parseHeapDump :: Parser a -> Parser (Tag a)
parseHeapDump = undefined

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
              | SubTagClassDump a Word32 a a a a a a Word32 [ConstantPoolRecord] [StaticField] [InstanceField]
              | SubTagInstanceDump a Word32 a Word32 [Word8]
              | SubTagObjectArrayDump a Word32 Word32 a [a]
              | SubTagPrimitiveArrayDump a Word32 Word32 Word8 [Word8] deriving(Show)

data ConstantPoolRecord = ConstantPoolRecord8 Word8
                        | ConstantPoolRecord16 Word16
                        | ConstantPoolRecord32 Word32
                        | ConstantPoolRecord64 Word64 deriving(Show)

data StaticField = StaticField8 Word8 | StaticField16 Word16 | StaticField32 Word32 | StaticField64 Word64 deriving(Show)

data InstanceField = InstanceField8 Word8 | InstanceField16 Word16 | InstanceField32 Word32 | InstanceField64 Word64 deriving(Show)

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
