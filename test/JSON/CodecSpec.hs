{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module JSON.CodecSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import JSON.Codec as JSON
import Test.Syd

spec :: Spec
spec = do
  describe "Bool" $ do
    parseSuccessSpec "true" True
    parseSuccessSpec "false" False
    parseFailSpec @Bool "no"

  describe "Text" $ do
    parseSuccessSpec @Text "\"Hello world!\"" "Hello world!"
    parseSuccessSpec @Text "\"\\\"" "\""
    parseSuccessSpec @Text "\"\\b\"" "\b"
    parseSuccessSpec @Text "\"\\f\"" "\f"
    parseSuccessSpec @Text "\"\\n\"" "\n"
    parseSuccessSpec @Text "\"\\r\"" "\r"
    parseSuccessSpec @Text "\"\\t\"" "\t"
    parseSuccessSpec @Text "\"\\\\\"" "\\"
    parseSuccessSpec @Text "\"\\u1234\"" "\4660"
    parseFailSpec @Text "\"unclosed double quote"

-- equivalentToAesonSpec :: forall output. String -> Spec
-- equivalentToAesonSpec name = describe name $ do
--   it "produces the same output as aeson does" $ do
--     undefined

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
