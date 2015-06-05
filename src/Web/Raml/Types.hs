{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Raml.Types where

import Data.Text (Text)
import qualified Data.HashMap.Strict as H

type Error = String

type Uri = Text
type Path = Text

data RamlProtocol = HTTP | HTTPS
  deriving (Show, Eq, Ord)

data RamlParameterType =
   RamlParameterString
    { paramEnum :: Maybe [Text]
    , paramPattern :: Maybe Text
    , paramMaxLength :: Maybe Int
    , paramMinLength :: Maybe Int
    , paramExampleS :: Maybe Text
    , paramDefaultS :: Maybe Text }
  | RamlParameterInteger
    { paramMaximumI :: Maybe Int
    , paramMinimumI :: Maybe Int
    , paramExampleI :: Maybe Int
    , paramDefaultI :: Maybe Int }
  | RamlParameterNumber
    { paramMaximumN :: Maybe Double
    , paramMinimumN :: Maybe Double
    , paramExampleN :: Maybe Double
    , paramDefaultN :: Maybe Double }
  | RamlParameterDate
    -- TODO: Implement using date type
    { paramExampleD :: Maybe Text
    , paramDefaultD :: Maybe Text }
  | RamlParameterBoolean
    { paramExampleB :: Maybe Bool
    , paramDefaultB :: Maybe Bool }
  | RamlParameterFile
    -- TODO: Do example and default make sense for files?
    -- Ask RAML org?
    { paramExample :: Maybe Text
    , paramDefault :: Maybe Text
  } deriving (Show, Eq)

data RamlParameter = RamlParameter
  { paramDisplayName :: Maybe Text
  , paramDescription :: Maybe Text
  , paramRepeat :: Maybe Bool
  , paramRequired :: Maybe Bool
  , paramType :: RamlParameterType
  } deriving (Show, Eq)

data RamlTrait = RamlTrait
  deriving (Show, Eq)

data RamlSecurityScheme = RamlSecurityScheme
  deriving (Show, Eq)

data RamlBody = RamlBody
  { bodySchema :: Maybe Text
  , bodyExample :: Maybe Text
  , bodyFormParameters :: Maybe (H.HashMap Text RamlParameter)
  } deriving (Show, Eq)

data RamlResponse = RamlResponse
  { respBody :: Maybe (H.HashMap Text RamlBody)
  , respDescription :: Maybe Text
  , respHeaders :: Maybe (H.HashMap Text RamlParameter)
  } deriving (Show, Eq)

data RamlMethod = RamlMethod
  { methDescription :: Maybe Text
  , methHeaders :: Maybe (H.HashMap Text RamlParameter)
  , methProtocols :: Maybe [RamlProtocol]
  , methQueryParameters :: Maybe (H.HashMap Text RamlParameter)
  , methBody :: Maybe (H.HashMap Text RamlBody)
  , methResponses :: Maybe (H.HashMap Text RamlResponse)
  , methIs :: Maybe [RamlTrait]
  --, methSecuredBy :: Maybe [RamlSecurityScheme]
  } deriving (Show, Eq)

data RamlResource = RamlResource
  { resDisplayName :: Maybe Text
  , resDescription :: Maybe Text
  , resUriParameters :: Maybe (H.HashMap Text RamlParameter)
  , resBaseUriParameters :: Maybe (H.HashMap Text RamlParameter)
  -- TODO: Generalize or support all HTTP std. methods?
  , resPost :: Maybe RamlMethod
  , resGet :: Maybe RamlMethod
  , resPut :: Maybe RamlMethod
  , resDelete :: Maybe RamlMethod
  , resResources :: H.HashMap Path RamlResource
  , resIs :: Maybe [RamlTrait]
  } deriving (Show, Eq)

data RamlDocumentation = RamlDocumentation
  { docTitle :: Text
  , docContent :: Text
  } deriving (Show, Eq, Ord)

data Raml = Raml
  { ramlTitle :: Text
  , ramlVersion :: Maybe Text
  -- TODO: Type for parameterized paths?
  , ramlBaseUri :: Maybe Uri
  -- TODO: Type for parameterized paths?
  , baseUriParameters :: Maybe (H.HashMap Text RamlParameter)
  -- TODO: Remember to parse from baseUri if present
  , ramlProtocols :: Maybe [RamlProtocol]
  -- TODO: Check against valid types in spec, better type than plain Text?
  , ramlMediaType :: Maybe Text
  , ramlDocumentation :: Maybe [RamlDocumentation]
  , ramlResources :: H.HashMap Path RamlResource
  --, ramlSchemas :: Maybe (H.HashMap Text Text)
  --, ramlResourceTypes :: Maybe [RamlResourceType]
  --, ramlTraits :: Maybe (H.HashMap Text RamlTrait)
  --, ramlSecuritySchemes :: Maybe (H.HashMap Text RamlSecurityScheme)
  } deriving (Show, Eq)


