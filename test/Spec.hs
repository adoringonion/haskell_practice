import Test.HUnit
import Data.Hjq as Hjq
import Data.Hjq.Parser

main :: IO ()
main = do
    runTestTT $ TestList
        [ jqFilterParserTest 
        , jqFilterParserSpacesTest
        ]
    return ()


jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [ "jqFilterParser test 1" ~: Hjq.parseJqFilter "." ~?= Right JqNil
    , "jqFilterParser test 2" ~: Hjq.parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: Hjq.parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: Hjq.parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: Hjq.parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
    [ "jqFilterParser test 1" ~: Hjq.parseJqFilter " . " ~?= Right JqNil
    , "jqFilterParser test 2" ~: Hjq.parseJqFilter " . [ 0 ] " ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: Hjq.parseJqFilter " . fieldName " ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: Hjq.parseJqFilter " . [ 0 ] . fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: Hjq.parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]