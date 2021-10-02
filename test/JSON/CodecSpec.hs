{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module JSON.CodecSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)
import JSON.Codec as JSON
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Bool" $ do
    parseSuccessSpec "true" True
    parseSuccessSpec "false" False
    parseFailSpec @Bool "no"
    roundtripSpec @Bool
    equivalentToAesonSpec @Bool

  describe "Text" $ do
    parseSuccessSpec @Text "\"Hello world!\"" "Hello world!"
    parseSuccessSpec @Text "\"\"" ""
    parseSuccessSpec @Text "\"\\\"" "\""
    parseSuccessSpec @Text "\"\\b\"" "\b"
    parseSuccessSpec @Text "\"\\f\"" "\f"
    parseSuccessSpec @Text "\"\\n\"" "\n"
    parseSuccessSpec @Text "\"\\r\"" "\r"
    parseSuccessSpec @Text "\"\\t\"" "\t"
    parseSuccessSpec @Text "\"\\\\\"" "\\"
    parseSuccessSpec @Text "\"\\u1234\"" "\4660"
    parseFailSpec @Text "\"unclosed double quote"
  -- roundtripSpec @Text
  -- equivalentToAesonSpec @Text

  describe "Thing" $
    roundtripSpec @Thing

data Thing = Thing {thingBool :: !Bool, thingText :: !Text}
  deriving (Show, Eq, Generic)

instance Validity Thing

instance GenValid Thing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Thing where
  codec =
    object $
      Thing
        <$> field "bool" .= thingBool
        <*> field "text" .= thingText

equivalentToAesonSpec :: forall value. (Show value, GenValid value, Aeson.ToJSON value, HasCodec value) => Spec
equivalentToAesonSpec = do
  it "produces the same output as aeson does" $
    forAllValid $ \(value :: value) ->
      let rendered = render value
          renderedViaAeson = Aeson.encode value
       in rendered `shouldBe` renderedViaAeson

roundtripSpec :: forall value. (Show value, Eq value, GenValid value, HasCodec value) => Spec
roundtripSpec = it "roundtrips" $
  forAllValid $ \expected ->
    let rendered = render expected
     in case parseEither @value rendered of
          Left err -> expectationFailure err
          Right actual -> context (show rendered) $ actual `shouldBe` expected

parseSuccess :: (Show output, Eq output, HasCodec output) => LB.ByteString -> output -> IO ()
parseSuccess input expected = case JSON.parseEither input of
  Left err -> expectationFailure err
  Right actual -> actual `shouldBe` expected

parseSuccessSpec :: (Show output, Eq output, HasCodec output) => LB.ByteString -> output -> Spec
parseSuccessSpec input output = it ("can parse " <> show input <> " into " <> show output) $ parseSuccess input output

parseFail :: forall output. (Show output, HasCodec output) => LB.ByteString -> IO ()
parseFail input = case JSON.parseEither @output input of
  Left _ -> pure ()
  Right actual -> expectationFailure $ "should have failed, but got this instead: " <> ppShow actual

parseFailSpec :: forall output. (Show output, HasCodec output) => LB.ByteString -> Spec
parseFailSpec input = it ("cannot parse " <> show input) $ parseFail @output input
