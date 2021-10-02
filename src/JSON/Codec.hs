{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Codec where

import Data.Attoparsec.ByteString.Char8 (char, hexadecimal)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Word

-- TODO find a better name for this?
data Codec input output where
  NullCodec :: Codec () ()
  BoolCodec :: Codec Bool Bool
  StringCodec :: Codec Text Text
  PureCodec :: output -> Codec input output
  ApCodec :: Codec input (output -> newOutput) -> Codec input output -> Codec input newOutput
  BimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput

instance Functor (Codec input) where
  fmap = fmapCodec

fmapCodec :: (oldOutput -> newOutput) -> Codec input oldOutput -> Codec input newOutput
fmapCodec f = BimapCodec f id

comapCodec :: (newInput -> oldInput) -> Codec oldInput output -> Codec newInput output
comapCodec g = BimapCodec id g

bimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput
bimapCodec = BimapCodec

instance Applicative (Codec input) where
  pure = pureCodec
  (<*>) = apCodec

pureCodec :: output -> Codec input output
pureCodec = PureCodec

apCodec :: Codec input (output -> newOutput) -> Codec input output -> Codec input newOutput
apCodec = ApCodec

boolCodec :: Codec Bool Bool
boolCodec = BoolCodec

textCodec :: Codec Text Text
textCodec = StringCodec

codecParser :: Codec void output -> Parser output
codecParser = go
  where
    go :: Codec void output -> Parser output
    go = \case
      NullCodec -> nullParser
      BoolCodec -> boolParser
      StringCodec -> stringParser
      PureCodec output -> pure output
      ApCodec functionCodec inputCodec -> go functionCodec <*> go inputCodec
      BimapCodec f _ c -> f <$> go c

nullParser :: Parser ()
nullParser = () <$ string "null"

boolParser :: Parser Bool
boolParser =
  choice
    [ True <$ string "true",
      False <$ string "false"
    ]

stringParser :: Parser Text
stringParser = do
  char '\"'
  nextChar <- anyWord8
  -- TODO split off a lazy text builder?
  LT.toStrict . LTB.toLazyText <$> go nextChar
  where
    go :: Word8 -> Parser LTB.Builder
    go = \case
      -- " (Double quote)
      34 -> pure mempty
      -- \ Backslash
      92 -> anyWord8 >>= goBackslash
      w -> (LTB.singleton (Char.chr (fromIntegral w)) <>) <$> (anyWord8 >>= go)

    goBackslash :: Word8 -> Parser LTB.Builder
    goBackslash = \case
      -- " (Double quote)
      34 -> pure (LTB.singleton '\"')
      -- \ Backslash
      92 -> (LTB.singleton '\\' <>) <$> (anyWord8 >>= go)
      -- / Slash
      47 -> (LTB.singleton '/' <>) <$> (anyWord8 >>= go)
      -- \b Backspace
      98 -> (LTB.singleton '\b' <>) <$> (anyWord8 >>= go)
      -- \f Formfeed
      102 -> (LTB.singleton '\f' <>) <$> (anyWord8 >>= go)
      -- \n Linefeed
      110 -> (LTB.singleton '\n' <>) <$> (anyWord8 >>= go)
      -- \r Carriage return
      114 -> (LTB.singleton '\r' <>) <$> (anyWord8 >>= go)
      -- \t Tab
      116 -> (LTB.singleton '\t' <>) <$> (anyWord8 >>= go)
      -- \ubeef Unicode character
      117 -> goUnicode
      _ -> fail "unclosed backslash"

    goUnicode :: Parser LTB.Builder
    goUnicode = do
      let hexChar = do
            w <- anyWord8
            pure $
              Char.digitToInt $ case w of
                48 -> '0'
                49 -> '1'
                50 -> '2'
                51 -> '3'
                52 -> '4'
                53 -> '5'
                54 -> '6'
                55 -> '7'
                56 -> '8'
                57 -> '9'
                97 -> 'a'
                98 -> 'b'
                99 -> 'c'
                100 -> 'd'
                101 -> 'e'
                102 -> 'f'

      h1 <- hexChar
      h2 <- hexChar
      h3 <- hexChar
      h4 <- hexChar
      (LTB.singleton (Char.chr (4096 * h1 + 256 * h2 + 16 * h3 + h4)) <>) <$> (anyWord8 >>= go)

class HasCodec domainType where
  codec :: Codec domainType domainType

instance HasCodec Bool where
  codec = boolCodec

instance HasCodec Text where
  codec = textCodec

parseResult :: HasCodec output => LB.ByteString -> Result output
parseResult = parse (codecParser codec)

parseMaybe :: HasCodec output => LB.ByteString -> Maybe output
parseMaybe = maybeResult . parseResult

parseEither :: HasCodec output => LB.ByteString -> Either String output
parseEither = eitherResult . parseResult
