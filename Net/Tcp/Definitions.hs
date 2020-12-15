{-# LANGUAGE NoImplicitPrelude #-}
module Net.Tcp.Definitions (
    TcpConnection (..)
    , reverse
    , nameFromTcpConnection
)

where

import Prelude
import Net.IP
import Data.Aeson
import Data.Word (Word8, Word16, Word32)
import GHC.Generics

{- Describe a TCP connection, possibly an Mptcp subflow
  The equality implementation ignores several fields
-}
data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: IP -- ^Source ip
  , dstIp :: IP -- ^Destination ip
  , srcPort :: Word16  -- ^ Source port
  , dstPort :: Word16  -- ^Destination port
  , priority :: Maybe Word8 -- ^subflow priority
  , localId :: Word8  -- ^ Convert to AddressFamily
  , remoteId :: Word8
  -- TODO remove could be deduced from srcIp / dstIp ?
  , subflowInterface :: Maybe Word32 -- ^Interface of Maybe ? why a maybe ?
  -- add TcpMetrics member
  -- , tcpMetrics :: Maybe [SockDiagExtension]  -- ^Metrics retrieved from kernel

} deriving (Show, Generic, Ord)


nameFromTcpConnection :: TcpConnection -> String
nameFromTcpConnection con =
  show (srcIp con) ++ ":" ++ show (srcPort con) ++ " -> " ++ show (dstIp con) ++ ":" ++ show (dstPort con)


reverseConnection :: TcpConnection -> TcpConnection
reverseConnection con = con {
  srcIp = dstIp con
  , dstIp = srcIp con
  , srcPort = dstPort con
  , dstPort = srcPort con
  , priority = Nothing
  , localId = remoteId con
  , remoteId = localId con
  , subflowInterface = Nothing
}


instance FromJSON TcpConnection
instance ToJSON TcpConnection

-- ignore the rest
instance Eq TcpConnection where
  x == y = srcIp x == srcIp y && dstIp x == dstIp y
            && srcPort x == srcPort y && dstPort x == dstPort y
  -- /= = not ==


