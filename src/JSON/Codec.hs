{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Codec where

import Control.Monad
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import Data.List (intersperse)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Word
import Text.Printf

-- TODO find a better name for this?
data Codec input output where
  NullCodec :: Codec () ()
  BoolCodec :: Codec Bool Bool
  StringCodec :: Codec Text Text
  ObjectCodec :: ObjectCodec value value -> Codec value value
  PureCodec :: output -> Codec input output
  BimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput

instance Functor (Codec input) where
  fmap = fmapCodec

fmapCodec :: (oldOutput -> newOutput) -> Codec input oldOutput -> Codec input newOutput
fmapCodec f = BimapCodec f id

comapCodec :: (newInput -> oldInput) -> Codec oldInput output -> Codec newInput output
comapCodec g = BimapCodec id g

bimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput
bimapCodec = BimapCodec

data ObjectCodec input output where
  KeyCodec :: Text -> Codec input output -> ObjectCodec input output
  PureObjectCodec :: output -> ObjectCodec input output
  BimapObjectCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> ObjectCodec oldInput oldOutput -> ObjectCodec newInput newOutput
  ApObjectCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput

fmapObjectCodec :: (oldOutput -> newOutput) -> ObjectCodec input oldOutput -> ObjectCodec input newOutput
fmapObjectCodec f = BimapObjectCodec f id

comapObjectCodec :: (newInput -> oldInput) -> ObjectCodec oldInput output -> ObjectCodec newInput output
comapObjectCodec g = BimapObjectCodec id g

bimapObjectCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> ObjectCodec oldInput oldOutput -> ObjectCodec newInput newOutput
bimapObjectCodec = BimapObjectCodec

object :: ObjectCodec value value -> Codec value value
object = ObjectCodec

instance Functor (ObjectCodec input) where
  fmap = fmapObjectCodec

instance Applicative (ObjectCodec input) where
  pure = pureObjectCodec
  (<*>) = apObjectCodec

instance HasCodec output => IsString (ObjectCodec output output) where
  fromString = field . fromString

(.=) :: ObjectCodec oldInput output -> (newInput -> oldInput) -> ObjectCodec newInput output
(.=) = flip comapObjectCodec

field :: HasCodec output => Text -> ObjectCodec output output
field k = KeyCodec k codec

pureObjectCodec :: output -> ObjectCodec input output
pureObjectCodec = PureObjectCodec

apObjectCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput
apObjectCodec = ApObjectCodec

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
      ObjectCodec oc -> objectCodecParser oc
      PureCodec v -> pure v
      BimapCodec f _ c -> f <$> go c

objectCodecParser :: ObjectCodec void output -> Parser output
objectCodecParser oc = char '{' *> go oc <* char '}'
  where
    go :: ObjectCodec void output -> Parser output
    go = \case
      KeyCodec _ _ -> undefined -- TODO
      PureObjectCodec output -> pure output
      ApObjectCodec functionCodec inputCodec -> go functionCodec <*> go inputCodec
      BimapObjectCodec f _ c -> f <$> go c

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
  void $ char '\"'
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
            -- TODO also upper case letters.
            Char.digitToInt <$> case w of
              48 -> pure '0'
              49 -> pure '1'
              50 -> pure '2'
              51 -> pure '3'
              52 -> pure '4'
              53 -> pure '5'
              54 -> pure '6'
              55 -> pure '7'
              56 -> pure '8'
              57 -> pure '9'
              97 -> pure 'a'
              98 -> pure 'b'
              99 -> pure 'c'
              100 -> pure 'd'
              101 -> pure 'e'
              102 -> pure 'f'
              _ -> fail "expected a hex char"
      h1 <- hexChar
      h2 <- hexChar
      h3 <- hexChar
      h4 <- hexChar
      (LTB.singleton (Char.chr (16 ^ (3 :: Int) * h1 + 16 ^ (2 :: Int) * h2 + 16 * h3 + h4)) <>) <$> (anyWord8 >>= go)

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

render :: HasCodec input => input -> LB.ByteString
render = renderViaCodec codec

renderViaCodec :: Codec input void -> input -> LB.ByteString
renderViaCodec c input = BB.toLazyByteString $ codecBuilder c input

codecBuilder :: Codec input void -> input -> BB.Builder
codecBuilder = flip go
  where
    go :: input -> Codec input void -> BB.Builder
    go input = \case
      NullCodec -> nullBuilder
      BoolCodec -> boolBuilder input
      StringCodec -> stringBuilder input
      ObjectCodec oc -> objectCodecBuilder oc input
      PureCodec _ -> mempty
      BimapCodec _ g oc -> go (g input) oc

objectCodecBuilder :: ObjectCodec input void -> input -> BB.Builder
objectCodecBuilder oc i = BB.char7 '{' <> renderKeyVals (go i oc) <> BB.char7 '}'
  where
    -- TODO use a difference list?
    go :: input -> ObjectCodec input void -> [(Text, BB.Builder)]
    go input = \case
      KeyCodec k c -> [(k, codecBuilder c input)]
      ApObjectCodec oc1 oc2 -> go input oc1 <> go input oc2
      PureObjectCodec _ -> mempty
      BimapObjectCodec _ g c -> go (g input) c

renderKeyVals :: [(Text, BB.Builder)] -> BB.Builder
renderKeyVals = mconcat . intersperse (BB.char7 ',') . map (uncurry go)
  where
    go :: Text -> BB.Builder -> BB.Builder
    go key value = stringBuilder key <> BB.char7 ':' <> value

nullBuilder :: BB.Builder
nullBuilder = "null"

boolBuilder :: Bool -> BB.Builder
boolBuilder = \case
  True -> "true"
  False -> "false"

stringBuilder :: Text -> BB.Builder
stringBuilder = (BB.char7 '"' <>) . (<> BB.char7 '"') . foldMap go . T.unpack
  where
    go :: Char -> BB.Builder
    go = \case
      '"' -> BB.char7 '\\' <> BB.char7 '"'
      '\\' -> BB.char7 '\\' <> BB.char7 '\\'
      '\b' -> BB.char7 '\\' <> BB.char7 'b'
      '\f' -> BB.char7 '\\' <> BB.char7 'f'
      '\n' -> BB.char7 '\\' <> BB.char7 'n'
      '\r' -> BB.char7 '\\' <> BB.char7 'r'
      '\t' -> BB.char7 '\\' <> BB.char7 't'
      c
        | c <= '\127' -> BB.char7 c
        | c <= '\255' -> BB.char7 '\\' <> BB.string7 (printf "%d" (Char.ord c))
        | otherwise -> BB.char7 '\\' <> BB.char7 'u' <> BB.string7 (printf "%04x" (Char.ord c))
