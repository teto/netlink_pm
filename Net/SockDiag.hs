{-|
    ipSrc <- IPAddress . pack <$> replicateM (4*8) getWord8
    -- ipSrc = IPAddress . pack <$> replicateM (4*8) getWord8
    ipDst <- IPAddress . pack <$> replicateM (4*8) getWord8 
Module      : IDiag
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Net.SockDiag (
  SockDiagMsg (..)
  , SockDiagExtension (..)
  , genQueryPacket
  , loadExtension
  , showExtension
  , connectionFromDiag
  -- WARN dont use it will be removed
  , enumsToWord
) where

-- import Generated
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude hiding (length, concat, init)

import Data.Maybe (fromJust)
import Data.Either (fromRight)

import Data.Serialize
import Data.Serialize.Get ()
import Data.Serialize.Put ()

-- import Control.Monad (replicateM)

import System.Linux.Netlink
import System.Linux.Netlink.Constants

import qualified Data.Bits as B
import Data.Bits ((.|.))
import qualified Data.Map as Map
import Data.ByteString (ByteString, pack)
import Data.ByteString.Char8 as C8 (unpack, init)
import Net.IPAddress
import Net.IP ()
-- import Net.IPv4
import Net.Tcp
import Net.SockDiag.Constants

--
-- import Data.BitSet.Word

-- requires cabal as a dep
-- import Distribution.Utils.ShortText (decodeStringUtf8)
import GHC.Generics

-- iproute uses this seq number #define MAGIC_SEQ 123456
-- TODO we could remove it
magicSeq :: Word32
magicSeq = 123456


-- TODO provide constructor from Cookie
-- and one fronConnection
-- {| InetDiagFromCookie Word64
--
-- |}
data InetDiagSockId  = InetDiagSockId  {
  idiag_sport :: Word16  -- ^Source port
  , idiag_dport :: Word16  -- ^Destination port

  -- Just be careful that this is a fixed size regardless of family
  -- __be32  idiag_src[4];
  -- __be32  idiag_dst[4];
  -- we don't know yet the address family
  , idiag_src :: ByteString
  , idiag_dst :: ByteString

  , idiag_intf :: Word32    -- ^Interface id
  , idiag_cookie :: Word64  -- ^To specifically request an sockid

} deriving (Eq, Show)

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


-- TODO we need a way to rebuild from the integer to the enum
class Enum2Bits a where
  -- toBits :: [a] -> Word32
  shiftL :: a -> Word32

instance Enum2Bits TcpState where
  shiftL state = B.shiftL 1 (fromEnum state)

instance Enum2Bits SockDiagExtensionId where
  shiftL state = B.shiftL 1 (fromEnum state - 1)



enumsToWord :: Enum2Bits a => [a] -> Word32
enumsToWord [] = 0
enumsToWord (x:xs) = (shiftL x) .|. (enumsToWord xs)

-- TODO use bitset package ? but broken
wordToEnums :: Enum2Bits a =>  Word32 -> [a]
wordToEnums  _ = []

{- | This generates a response of inet_diag_msg
-}
data SockDiagMsg = SockDiagMsg {
  idiag_family :: AddressFamily  -- ^
  , idiag_state :: Word8 -- ^Bitfield matching the request
  , idiag_timer :: Word8
  , idiag_retrans :: Word8
  , idiag_sockid :: InetDiagSockId
  , idiag_expires :: Word32
  , idiag_rqueue :: Word32
  , idiag_wqueue :: Word32
  , idiag_uid :: Word32
  , idiag_inode :: Word32
} deriving (Eq, Show)

{-# LANGUAGE FlexibleInstances #-}

-- TODO this generates the  error "Orphan instance: instance Convertable [TcpState]"
-- instance Convertable [TcpState] where
--   getPut = putStates
--   getGet _ = return []

putStates :: [TcpState] -> Put
putStates states = putWord32host $ enumsToWord states


instance Convertable SockDiagMsg where
  getPut = putSockDiagMsg
  getGet _ = getSockDiagMsg

-- TODO rename to a TCP one ? SockDiagRequest
data SockDiagRequest = SockDiagRequest {
  sdiag_family :: Word8 -- ^AF_INET6 or AF_INET (TODO rename)
-- It should be set to the appropriate IPPROTO_* constant for AF_INET and AF_INET6, and to 0 otherwise.
  , sdiag_protocol :: Word8 -- ^IPPROTO_XXX always TCP ?
  -- IPv4/v6 specific structure
  -- Bitset
  , idiag_ext :: [SockDiagExtensionId] -- ^query extended info (word8 size)
  -- , req_pad :: Word8        -- ^ padding for backwards compatibility with v1

  -- in principle, any kind of state, but for now we only deal with TcpStates
  , idiag_states :: [TcpState] -- ^States to dump (based on TcpDump) Word32
  , diag_sockid :: InetDiagSockId -- ^inet_diag_sockid 
} deriving (Eq, Show)

{- |Typeclase used by the system. Basically 'Storable' for 'Get' and 'Put'
getGet Returns a 'Get' function for the convertable.
The MessageType is passed so that the function can parse different data structures
based on the message type.
-}
-- class Convertable a where
--   getGet :: MessageType -> Get a -- ^get a 'Get' function for the static data
--   getPut :: a -> Put -- ^get a 'Put' function for the static data
instance Convertable SockDiagRequest where
  getPut = putSockDiagRequestHeader
  -- MessageType
  getGet _ = getSockDiagRequestHeader

-- |'Get' function for 'GenlHeader'
-- applicative style Trade <$> getWord32le <*> getWord32le <*> getWord16le
getSockDiagRequestHeader :: Get SockDiagRequest
getSockDiagRequestHeader = do
    addressFamily <- getWord8 -- AF_INET for instance
    protocol <- getWord8
    extended <- getWord32host
    _pad <- getWord8
    -- TODO discarded later
    states <- getWord32host
    _sockid <- getInetDiagSockid
    -- TODO reestablish states
    return $ SockDiagRequest addressFamily protocol
      (wordToEnums extended :: [SockDiagExtensionId]) (wordToEnums states :: [TcpState])  _sockid

-- |'Put' function for 'GenlHeader'
putSockDiagRequestHeader :: SockDiagRequest -> Put
putSockDiagRequestHeader request = do
  -- let states = enumsToWord $ idiag_states request
  putWord8 $ sdiag_family request
  putWord8 $ sdiag_protocol request
  -- extended todo use Enum2Bits
  --putWord32host $ enumsToWord states
  putWord8 ( fromIntegral (enumsToWord $ idiag_ext request) :: Word8)
  putWord8 0  -- padding ?
  -- TODO check endianness
  putStates $ idiag_states request
  putInetDiagSockid $ diag_sockid request

-- |Converts a generic SockDiagMsg into a TCP connection
connectionFromDiag :: SockDiagMsg
              -> TcpConnection
connectionFromDiag msg =
  let sockid = idiag_sockid msg in
  TcpConnection {
    srcIp = fromRight (error "no default") (getIPFromByteString (idiag_family msg) (idiag_src sockid))
    , dstIp = fromRight (error "no default") (getIPFromByteString (idiag_family msg) (idiag_dst sockid))
    , srcPort = idiag_sport sockid
    , dstPort = idiag_dport sockid
    , priority = Nothing
    , localId = 0
    , remoteId = 0
    , subflowInterface = Nothing
  }

-- | Serialize SockDiagMsg
-- Usually accompanied with attributes ?
getSockDiagMsg :: Get SockDiagMsg
getSockDiagMsg  = do
    family <- getWord8
    state <- getWord8
    timer <- getWord8
    retrans <- getWord8

    _sockid <- getInetDiagSockid
    expires <- getWord32host
    rqueue <- getWord32host
    wqueue <- getWord32host
    uid <- getWord32host
    inode <- getWord32host
    return$  SockDiagMsg (fromIntegral family) state timer retrans _sockid expires rqueue wqueue uid inode

putSockDiagMsg :: SockDiagMsg -> Put
putSockDiagMsg msg = do
  putWord8 $ fromIntegral $ fromEnum $ idiag_family msg
  putWord8 $ idiag_state msg
  putWord8 $ idiag_timer msg
  putWord8 $ idiag_retrans msg

  putInetDiagSockid $ idiag_sockid msg

  -- Network order
  putWord32le $ idiag_expires msg
  putWord32le $ idiag_rqueue msg
  putWord32le $ idiag_wqueue msg
  putWord32le $ idiag_uid msg
  putWord32le $ idiag_inode msg

-- TODO add support for OWDs
getInetDiagSockid :: Get InetDiagSockId
getInetDiagSockid  = do
    sport <- getWord16host
    dport <- getWord16host
    -- iterate/ grow
    _src <- getByteString (4*4)
    _dst <- getByteString (4*4)
    _intf <- getWord32host
    cookie <- getWord64host
    return $ InetDiagSockId sport dport _src _dst _intf cookie

-- | put addresses as bytestring since the family is not known yet
putInetDiagSockid :: InetDiagSockId -> Put
putInetDiagSockid cust = do
  -- we might need to clean up this a bit
  putWord16be $ idiag_sport cust
  putWord16be $ idiag_dport cust
  putByteString (idiag_src cust)
  putByteString (idiag_dst cust)
  putWord32host $ idiag_intf cust
  putWord64host $ idiag_cookie cust


getDiagVegasInfo :: Get SockDiagExtension
getDiagVegasInfo =
  TcpVegasInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6


{-|
Different answers described in include/uapi/linux/inet_diag.h
Please keep the spacing the same as 
-}
data SockDiagExtension =
  -- | Exact copy of kernel's struct tcp_info
  -- tcp_diag_get_info
  DiagTcpInfo {
  tcpi_state :: Word8,
  tcpi_ca_state :: Word8,
  tcpi_retransmits :: Word8,
  tcpi_probes :: Word8,
  tcpi_backoff :: Word8,
  tcpi_options :: Word8,
  tcpi_wscales :: Word8  -- ^both sender and receiver on 4 bits
  , tcpi_delivery_rate_app_limited :: Word8 -- ^but only first bit used

  , tcpi_rto :: Word32,
  tcpi_ato :: Word32,
  tcpi_snd_mss :: Word32,
  tcpi_rcv_mss :: Word32,

  tcpi_unacked :: Word32,
  tcpi_sacked :: Word32,
  tcpi_lost :: Word32,
  tcpi_retrans :: Word32,
  tcpi_fackets :: Word32,

  -- Time
  tcpi_last_data_sent :: Word32,
  tcpi_last_ack_sent :: Word32,
  tcpi_last_data_recv :: Word32,
  tcpi_last_ack_recv :: Word32,

  -- Metric
  tcpi_pmtu :: Word32,
  tcpi_rcv_ssthresh :: Word32,
  tcpi_rtt :: Word32,
  tcpi_rttvar :: Word32,
  tcpi_snd_ssthresh :: Word32,
  tcpi_snd_cwnd :: Word32,
  tcpi_advmss :: Word32,
  tcpi_reordering :: Word32,

  tcpi_rcv_rtt :: Word32,
  tcpi_rcv_space :: Word32

  , tcpi_total_retrans :: Word32

  , tcpi_pacing_rate :: Word64
  , tcpi_max_pacing_rate :: Word64
  , tcpi_bytes_acked :: Word64
  , tcpi_bytes_received :: Word64
  , tcpi_segs_out :: Word32
  , tcpi_segs_in :: Word32

  , tcpi_notsent_bytes :: Word32
  , tcpi_min_rtt :: Word32
  , tcpi_data_segs_in :: Word32
  , tcpi_data_segs_out :: Word32

  , tcpi_delivery_rate :: Word64

  , tcpi_busy_time :: Word64
  , tcpi_rwnd_limited :: Word64
  , tcpi_sndbuf_limited :: Word64

  , tcpi_delivered :: Word32
  , tcpi_delivered_ce :: Word32

  , tcpi_bytes_sent :: Word64
  , tcpi_bytes_retrans :: Word64
  , tcpi_dsack_dups :: Word32
  , tcpi_reord_seen :: Word32

  -- Extended version, hoping it doesn't break too much stuff
  , tcpi_snd_cwnd_clamp :: Word32
  , tcpi_fowd :: Word32
  , tcpi_bowd :: Word32

} | DiagExtensionMemInfo {
  idiag_rmem :: Word32  -- ^ Amount of data in the receive queue.
, idiag_wmem :: Word32  -- ^ amount of data that is queued by TCP but not yet sent.
, idiag_fmem :: Word32  -- ^ amount of memory scheduled for future use
, idiag_tmem :: Word32  -- ^ amount of data in send queue.
} |
  -- | Not exclusive to Vegas unlike the name indicates, mirrors tcpvegas_info
  TcpVegasInfo {
  tcpInfoVegasEnabled :: Word32
  , tcpInfoRttCount :: Word32
  , tcpInfoRtt :: Word32
  , tcpInfoMinrtt :: Word32
} | CongInfo String
  | SockDiagShutdown Word8
  -- Apparently used to pass BBR data
  | SockDiagMark Word32
  deriving (Show, Generic)

-- ideally we should be able to , Serialize
-- instance Convertable SockDiagExtension where
--   getGet _ = get
--   getPut = put


getTcpVegasInfo :: Get SockDiagExtension
getTcpVegasInfo = TcpVegasInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

getMemInfo :: Get SockDiagExtension
getMemInfo = DiagExtensionMemInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

getDiagMark :: Get SockDiagExtension
getDiagMark = SockDiagMark <$> getWord32host


getShutdown :: Get SockDiagExtension
getShutdown = SockDiagShutdown <$> getWord8

-- | Get congestion control name
getCongInfo :: Get SockDiagExtension
getCongInfo = do
    left <- remaining
    bs <- getByteString left
    return (CongInfo $ unpack $ init bs)


getDiagTcpInfo :: Get SockDiagExtension
getDiagTcpInfo = DiagTcpInfo <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host
  -- times
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host
  -- metrics
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host


  <*> getWord32host <*> getWord32host

  -- tcpi_total_retrans
  <*> getWord32host

  -- starts at tcpi_pacing_rate
  <*> getWord64host <*> getWord64host <*> getWord64host <*> getWord64host
  <*> getWord32host <*> getWord32host

  -- starts at tcpi_notsent_bytes
  <*> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

  -- tpci_delivery_rate
  <*> getWord64host

  <*> getWord64host <*> getWord64host<*> getWord64host

  -- tcpi_delivered
  <*> getWord32host <*> getWord32host

  -- tcpi_bytes_sent
  <*> getWord64host<*> getWord64host <*> getWord32host <*> getWord32host

  -- My custom addition to read the owds, it's an extra that should be removed 
  -- for a vanilla kernel
  <*> getWord32host <*> getWord32host <*> getWord32host

-- Sends a SockDiagRequest
-- expects INetDiag
-- TODO should take an Mptcp connection into account
-- We should use cookies later on
-- MaybeCookie ?
-- TcpConnection -- ^Connection we are requesting
-- #define SS_ALL ((1 << SS_MAX) - 1)
-- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
-- stateFilter = [TcpListen, TcpEstablished, TcpSynSent ]

-- InetDiagInfo
-- TODO we need to request more !
-- TODO if we have a cookie ignore the rest ?!
-- requestedInfo = InetDiagNone

showExtension :: SockDiagExtension -> String
showExtension (CongInfo cc) = "Using CC " ++ (show cc)
showExtension (TcpVegasInfo _ _ rtt minRtt) = "RTT=" ++ (show rtt) ++ " minRTT=" ++ show minRtt
showExtension (arg@DiagTcpInfo{}) = "TcpInfo: rtt/rttvar=" ++ show ( tcpi_rtt arg) ++ "/" ++ show ( tcpi_rttvar arg)
        ++ " snd_cwnd/ssthresh=" ++ show (tcpi_snd_cwnd arg) ++ "/" ++ show (tcpi_snd_ssthresh arg)
showExtension rest = show rest

{- Generate
  Check man sock_diag
-}
genQueryPacket :: (Either Word64 TcpConnection)
        -> [TcpState] -- ^Ignored when querying a single connection
        -> [SockDiagExtensionId] -- ^Queried values
        -> Packet SockDiagRequest
genQueryPacket selector tcpStatesFilter requestedInfo = let
  -- Mesge type / flags /seqNum /pid
  flags = (fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT)

  -- might be a trick with seqnum
  hdr = Header msgTypeSockDiag flags magicSeq 0

  diag_req = case selector of
    -- TODO
    Left cookie -> let
        bstr = pack $ replicate 128 (0 :: Word8)
      in
        InetDiagSockId 0 0 bstr bstr 0 cookie

    Right con -> let
        ipSrc = runPut $ putIPAddress (srcIp con)
        ipDst = runPut $ putIPAddress (dstIp con)
        ifIndex = subflowInterface con
        _cookie = 0 :: Word64
      in
        InetDiagSockId (srcPort con) (dstPort con) ipSrc ipDst (fromJust ifIndex) _cookie

  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo tcpStatesFilter diag_req
  in
    Packet hdr custom Map.empty

-- | to search for a specific connection
queryPacketFromCookie :: Word64 -> Packet SockDiagRequest
queryPacketFromCookie cookie =  genQueryPacket (Left cookie) [] []


loadExtension :: Int -> ByteString -> Maybe SockDiagExtension
loadExtension key value = let
  eExtId = (toEnum key :: SockDiagExtensionId)
  fn = case toEnum key of
    -- MessageType shouldn't matter anyway ?!
    InetDiagCong -> Just getCongInfo
    -- InetDiagNone -> Nothing
    InetDiagInfo -> Just getDiagTcpInfo
    InetDiagVegasinfo -> Just getTcpVegasInfo
    -- InetDiagTos -> Nothing
    -- InetDiagTclass -> Nothing
    -- InetDiagSkmeminfo -> Nothing
    InetDiagShutdown -> Just getShutdown
    InetDiagMeminfo  -> Just getMemInfo
    -- InetDiagDctcpinfo -> Nothing
    -- InetDiagProtocol -> Nothing
    -- InetDiagSkv6only -> Nothing
    -- InetDiagLocals -> Nothing
    -- InetDiagPeers -> Nothing
    -- InetDiagPad -> Nothing
    -- requires CAP_NET_ADMIN
    InetDiagMark -> Just getDiagMark
    -- InetDiagBbrinfo -> Nothing
    -- InetDiagClassId -> Nothing
    -- InetDiagMd5sig -> Nothing
    -- InetDiagMax -> Nothing
    _ -> Nothing
    -- _ -> case decode value of
                        -- Right x -> Just x
                        -- -- Left err -> error $ "fourre-tout error " ++ err
                        -- Left err -> Nothing

    in case fn of
      Nothing -> Nothing
      Just getFn -> case runGet getFn  value of
          Right x -> Just $ x
          Left err -> error $ "error decoding " ++ show eExtId ++ ":\n" ++ err

