{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.Hjq.Parser

main :: IO ()
main = do
    runTestTT $ TestList
        [ jqFilterParserTest 
        , jqFilterParserSpacesTest
        , jqQueryParserTest
        , jqQueryParserSpacesTest
        ]
    return ()


jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [ "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
    , "jqFilterParser test 2" ~: parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
    [ "jqFilterParser test 1" ~: parseJqFilter " . " ~?= Right JqNil
    , "jqFilterParser test 2" ~: parseJqFilter " . [ 0 ] " ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: parseJqFilter " . fieldName " ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: parseJqFilter " . [ 0 ] . fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
  [
    "jqQueryParser test 1" ~: parseJqQuery "[]" ~?= Right (JqQueryArray [])
  , "jqQueryParser test 2" ~: parseJqQuery "[.hoge,.piyo]" ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter
    (JqField "piyo" JqNil)])
  , "jqQueryParser test 3" ~: parseJqQuery "{\"hoge\":[],\"piyo\":[]}" ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
  ]

jqQueryParserSpacesTest :: Test
jqQueryParserSpacesTest = TestList
  [
    "jqQueryParser test 1" ~: parseJqQuery " [ ] " ~?= Right (JqQueryArray [])
  , "jqQueryParser test 2" ~: parseJqQuery " [ . hoge , . piyo ] " ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter
    (JqField "piyo" JqNil)])
  , "jqQueryParser test 3" ~: parseJqQuery "{ \"hoge\" : [ ] , \"piyo\" : [ ] } " ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
  ]

applyFilterTest :: Test
applyFilterTest = TestList
  [
    "applyFilter test 1" ~: applyFilter (unsafeParseFilter ".") testData ~?= Right testData
  , "applyFilter test 2" ~: (Just $ applyFilter (unsafeParseFilter ".string-field") testData)
    ~?= fmap Right (testData ^? key "string-field")
  , "applyFilter test 3" ~: (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-string") testData)
    ~?= fmap Right (testData ^? key "nested-field" . key "inner-string")
  , "applyFilter test 4" ~: (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-number") testData)
    ~?= fmap Right (testData ^? key "nested-field" . key "inner-number")
  , "applyFilter test 5" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[0]") testData)
    ~?= fmap Right (testData ^? key "array-field" . nth 0)
  , "applyFilter test 6" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[1]") testData)
    ~?= fmap Right (testData ^? key "array-field" . nth 1)
  , "applyFilter test 7" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[2].object-in-array") testData)
    ~?= fmap Right (testData ^? key "array-field" . nth 2 . key "object-in-array")
  ]

testData :: Value
testData = Object $ H.fromList
  [ ("string-field", String "string value")
  , ("nested-field", Object $ H.fromList
      [ ("inner-string", String "inner value")
      , ("inner-number", Number 100)
      ]
    )
  , ("array-field", Array $ V.fromList
      [ String "first field"
      , String "next field"
      , Object (H.fromList
        [  ("object-in-array", String "string value in object-in-array")  ]  )
      ]
    )
  ]

unsafeParseFilter :: Text -> JqFilter
unsafeParseFilter t = case parseJqFilter r of
  Right f -> f
  Left s -> error $ "PARSE FAILURE IN A TEST" ++ unpack s