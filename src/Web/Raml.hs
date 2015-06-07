{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Raml
  ( decode
  , decodeFile
  ) where

import Control.Applicative
import Data.Aeson hiding (decode)
import Data.Aeson.Types (Parser)
import Data.Yaml (decodeEither)
import Data.List (nub)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Traversable (mapM)

import Prelude hiding (mapM)

import Web.Raml.Types

toParameterType :: H.HashMap T.Text Value -> Maybe T.Text
                -> Parser RamlParameterType
toParameterType v Nothing = toParameterType v (Just "string")
toParameterType v (Just "string") = RamlParameterString
  <$> v .:? "enum"
  <*> v .:? "pattern"
  <*> v .:? "maxLength"
  <*> v .:? "minLength"
  <*> v .:? "example"
  <*> v .:? "default"
toParameterType v (Just "integer") = RamlParameterInteger
  <$> v .:? "maximum"
  <*> v .:? "minimum"
  <*> v .:? "example"
  <*> v .:? "default"
toParameterType v (Just "number") = RamlParameterNumber
  <$> v .:? "maximum"
  <*> v .:? "minimum"
  <*> v .:? "example"
  <*> v .:? "default"
toParameterType v (Just "date") = RamlParameterDate
  <$> v .:? "example"
  <*> v .:? "default"
toParameterType v (Just "boolean") = RamlParameterBoolean
  <$> v .:? "example"
  <*> v .:? "default"
toParameterType v (Just "file") = RamlParameterFile
  <$> v .:? "example"
  <*> v .:? "default"
toParameterType _ (Just s) = fail $ "Not a valid parameter type: " ++ show s

instance FromJSON RamlParameter where
    parseJSON (Object v) = do
      t <- v .:? "type"
      RamlParameter
        <$> v .:? "displayName"
        <*> v .:? "description"
        <*> v .:? "repeat"
        <*> v .:? "required"
        <*> (toParameterType v t)
    parseJSON m = fail $ "Not a valid body: " ++ show m

instance FromJSON RamlTrait where
    parseJSON _ = fail "Traits not yet supported"

instance FromJSON RamlSecurityScheme where
    parseJSON _ = fail "Security schemes not yet supported"

instance FromJSON RamlBody where
    parseJSON (Object v) = RamlBody
        <$> v .:? "schema"
        <*> v .:? "example"
        <*> v .:? "formParameters"
    parseJSON Null = parseJSON (Object H.empty)
    parseJSON m = fail $ "Not a valid body: " ++ show m

instance FromJSON RamlResponse where
    parseJSON (Object v) = RamlResponse
        <$> v .:? "body"
        <*> v .:? "description"
        <*> v .:? "headers"
    parseJSON m = fail $ "Not a valid response: " ++ show m

instance FromJSON RamlMethod where
    parseJSON (Object v) = RamlMethod
        <$> v .:? "description"
        <*> v .:? "headers"
        <*> nub' (v .:? "protocols")
        <*> v .:? "queryParameters"
        <*> v .:? "body"
        <*> v .:? "responses"
        <*> v .:? "is"
      where nub' = (fmap . fmap) nub
    parseJSON m = fail $ "Not a valid method: " ++ show m

instance FromJSON RamlResource where
    parseJSON (Object v) = RamlResource
        <$> v .:? "displayName"
        <*> v .:? "description"
        <*> v .:? "uriParameters"
        <*> v .:? "baseUriParameters"
        -- TODO: Make methods case insensitive
        <*> v .:? "get"
        <*> v .:? "post"
        <*> v .:? "put"
        <*> v .:? "delete"
        <*> toResource v
        <*> v .:? "is"
    parseJSON Null = parseJSON (Object H.empty)
    parseJSON m = fail $ "Not a valid resource: " ++ show m

instance FromJSON RamlProtocol where
    parseJSON (String "HTTP") = pure HTTP
    parseJSON (String "HTTPS") = pure HTTPS
    parseJSON m = fail $
      "Protocol must be HTTP or HTTPS, got: " ++ show m

instance FromJSON RamlDocumentation where
    parseJSON (Object v) = RamlDocumentation
        <$> v .: "title"
        <*> v .: "content"
    parseJSON m = fail $ "Not a valid documentation entry: " ++ show m

toResource :: H.HashMap T.Text Value -> Parser (H.HashMap T.Text RamlResource)
toResource m = mapM (parseJSON :: Value -> Parser RamlResource) $
  H.filterWithKey (\k _ -> T.head k == '/') m

instance FromJSON Raml where
    parseJSON (Object v) = Raml
        <$> v .: "title"
        <*> v .:? "version"
        <*> v .:? "baseUri"
        <*> v .:? "baseUriParameters"
        <*> nub' (v .:? "protocols")
        <*> v .:? "mediaType"
        <*> v .:? "documentation"
        <*> toResource v
      where nub' = (fmap . fmap) nub
    parseJSON m = fail $ "Not a valid RAML file: " ++ show m

-- | Deserialize a RAML document from a 'BS.ByteString'.
-- If this fails due to incomplete or invalid input, 'Left e' is
-- returned, containing an error message.
decode :: BS.ByteString -> Either Error Raml
decode = decodeEither

-- | Deserialize a RAML document from a file.
-- If this fails due to incomplete or invalid input, 'Left e' is
-- returned, containing an error message.
decodeFile :: String -> IO (Either Error Raml)
decodeFile f = do
  raml <- BS.readFile f
  return $ decode raml

