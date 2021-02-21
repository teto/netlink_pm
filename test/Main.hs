{-# LANGUAGE OverloadedStrings #-}
{-|
https://stackoverflow.com/questions/32913552/why-does-my-hunit-test-suite-pass-when-my-tests-fail
-}
module Main where

import System.Exit
import Test.HUnit
import Net.SockDiag
import Net.Mptcp
import Net.Tcp
import Net.IP
import Net.Bitset

import Net.IPv4 (localhost)
import Numeric (readHex)
import Data.Text (Text)
import qualified Data.Text as T

-- TODO reestablish this
testEmpty = TestCase $ assertEqual
  "Check"
  1
  ( enumsToWord [TcpEstablished] )

testCombo = TestCase $ assertEqual
  "Check"
  513
  ( enumsToWord [TcpEstablished, TcpListen] )

testComboReverse = TestCase $ assertEqual
  "Check"
  513
  ( enumsToWord [TcpEstablished, TcpListen] )


iperfConnection = TcpConnection {
        srcIp = fromIPv4 localhost
        , dstIp = fromIPv4 localhost
        , srcPort = 5000
        , dstPort = 1000
        -- placeholder values
        , priority = Nothing
        , subflowInterface = Nothing
        , localId = 0
        , remoteId = 0
    }

modifiedConnection = iperfConnection { subflowInterface = Just 0 }

filteredConnections :: [TcpConnection]
filteredConnections = [ iperfConnection ]


connectionFilter = TestCase $ assertBool
  "Check connection is in the list"
  (iperfConnection `elem` filteredConnections)




-- check we can read an hex from tshark
-- 0x00000012
--  returns a list of possible parses as (a,String) pairs.
loadTcpFlagsFromHex :: Text -> [TcpFlag]
loadTcpFlagsFromHex text = case readHex (T.unpack $ T.drop 2 text) of
  [(n, "")] -> fromBitMask n
  _ -> error $ "TcpFlags: could not parse " ++ T.unpack text


-- connectionFilter = TestCase $ assertEqual
--   "Check connection is in the list"
--   True
--   ( iperfConnection `elem` filteredConnections)

-- main :: IO Count
main = do

  results <- runTestTT $ TestList [
      TestLabel "subflow is correctly filtered" connectionFilter
      , TestCase $ assertBool "connection should be equal" (iperfConnection == iperfConnection)
      , TestCase $ assertEqual "connection should be equal despite different interfaces"
          iperfConnection modifiedConnection
      , TestCase $ assertBool "connection should be considered as in list"
          (modifiedConnection `elem` filteredConnections)
      -- , TestCase $ assertBool "connection should not be considered as in list"
      --     (modifiedConnection `notElem` filteredConnections)
      , TestList [
        TestCase $ assertEqual "to bitset " 2 (toBitMask [TcpFlagSyn])
        , TestCase $ assertEqual "Check tcp syn flags" [TcpFlagSyn]
          (loadTcpFlagsFromHex "0x00000002")
        , TestCase $ assertEqual "Check tcp syn/ack flags" [TcpFlagSyn, TcpFlagAck]
          (loadTcpFlagsFromHex "0x00000012")
      ]
    ]
  if errors results + failures results == 0 then
      exitSuccess
    else
      exitWith (ExitFailure 1)
