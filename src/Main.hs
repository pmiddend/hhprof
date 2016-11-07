module Main where

import Prelude(fromIntegral)
import Data.Either(Either(..))
import System.IO(IO,putStrLn)
import System.Environment(getArgs)
import Data.Eq((/=))
import Data.Function(($))
import Data.Monoid((<>))
import qualified Data.ByteString as BS
import Data.Word(Word32,Word64)
import Data.Attoparsec.ByteString(Parser,takeWhile,parseOnly,anyWord8)
import Data.Attoparsec.Binary(anyWord32be)
import Control.Monad(return)
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

data Tag = Utf8String 

tag :: Parser Tag
tag = 

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      putStrLn $ "parsing " <> x <> "\n"
      fileContents <- BS.readFile x
      case parseOnly headerParser fileContents of
        Left e -> putStrLn $ "error parsing: " <> e
        Right result -> putStrLn $ "success:\n" <> show result
    _ ->
      putStrLn $ "invalid input"
