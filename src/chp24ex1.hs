module Chp24Ex1 where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneOnly = one >> eof

oneStr = string "123" <|> string "12" <|> string "1"

oneStr' = char '1' >> char '2' >> char '3' >> eof

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwoOnly = oneTwo >> eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse2 :: Parser () -> IO ()
testParse2 p = print $ parseString p mempty "123"

testParseStr :: Parser String -> IO ()
testParseStr p = print $ parseString p mempty "1"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "ontTwo':"
  testParse oneTwo'
  pNL "oneOnly:"
  testParse2 oneOnly
  pNL "oneTwoOnly:"
  testParse2 oneTwoOnly
  pNL "oneStr:"
  testParseStr oneStr
  pNL "oneStr':"
  testParse2 oneStr'


