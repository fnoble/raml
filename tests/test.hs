{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as H

import Web.Raml
import Web.Raml.Types

main :: IO ()
main = do
  ts <- loadFiles ramlOutputs
  defaultMain $ unitTests ts

unitTests :: [(String, Either Error Raml, Either Error Raml)] -> TestTree
unitTests ts = testGroup "Tests" $ map testRaml ts

ramlOutputs :: [(String, Either Error Raml)]
ramlOutputs =
  [ ("tests/raml_data/pass1.raml", Right $ Raml
      { ramlTitle = "Test"
      , ramlVersion = Nothing
      , ramlBaseUri = Nothing
      , baseUriParameters = Nothing
      , ramlProtocols = Nothing
      , ramlMediaType = Nothing
      , ramlDocumentation = Nothing
      , ramlResources = H.empty
      })
  , ("tests/raml_data/fail1.raml", Left "key \"title\" not present")
  , ("tests/raml_data/pass2.raml", Right $ Raml
      { ramlTitle = "World Music API"
      , ramlVersion = Just "v1"
      , ramlBaseUri = Just "http://example.api.com/{version}"
      , baseUriParameters = Nothing
      , ramlProtocols = Just [HTTPS]
      , ramlMediaType = Just "application/json"
      , ramlDocumentation = Just
          [ RamlDocumentation
            { docTitle = "Test"
            , docContent = "Testing 1 2 3\n" }
          , RamlDocumentation
            { docTitle = "Test 2"
            , docContent = "Hello there!\n" } ]
      , ramlResources = H.fromList
          [ ("/songs", RamlResource
            { resDisplayName = Just "Songs!"
            , resDescription = Just "All the songs"
            , resUriParameters = Nothing
            , resBaseUriParameters = Nothing
            , resPost = Nothing
            , resGet = Nothing
            , resPut = Nothing
            , resDelete = Nothing
            , resResources = H.empty
            , resIs = Nothing } ) ]
      })
  ]

loadFiles :: [(String, Either Error Raml)] -> IO [(String, Either Error Raml, Either Error Raml)]
loadFiles = mapM $ \(f, o) -> do
  raml <- decodeFile f
  return (f, o, raml)

testRaml :: (String, Either Error Raml, Either Error Raml) -> TestTree
testRaml (f, o, raml) = testCase f $ raml @?= o


