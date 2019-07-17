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
  InetDiagMsg (..)
  , IDiagExtension (..)
  , genQueryPacket
  , loadExtension
  , showExtension
) where

-- import Generated
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude hiding (length, concat)
import Prelude hiding (length, concat)

import Data.Maybe (fromJust)

import Data.Serialize
import Data.Serialize.Get ()
import Data.Serialize.Put ()

-- import Control.Monad (replicateM)

import System.Linux.Netlink
import System.Linux.Netlink.Constants
-- For TcpState, FFI generated
import Generated
-- (IDiagExt, TcpState, msgTypeSockDiag)

import qualified Data.Bits as B
import Data.Bits ((.|.))
import qualified Data.Map as Map
import Data.ByteString ()
import Data.ByteString.Char8 as C8 (unpack)
import Net.IPAddress
import Net.IP ()
-- import Net.IPv4
import Net.Tcp
import Data.ByteString (ByteString, pack, )

-- requires cabal as a dep
-- import Distribution.Utils.ShortText (decodeStringUtf8)
import GHC.Generics

-- iproute uses this seq number #define MAGIC_SEQ 123456
magicSeq :: Word32
magicSeq = 123456


-- TODO provide constructor from Cookie
-- and one fronConnection
-- {| InetDiagFromCookie Word64
--
-- |}
data InetDiagSockId  = InetDiagSockId  {
  idiag_sport :: Word16
  , idiag_dport :: Word16

  -- Just be careful that this is a fixed size regardless of family
  -- __be32  idiag_src[4];
  -- __be32  idiag_dst[4];
  -- we don't know yet the address family
  , idiag_src :: ByteString
  , idiag_dst :: ByteString

  , idiag_intf :: Word32
  , idiag_cookie :: Word64

} deriving (Eq, Show)

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


-- TODO we need a way to rebuild from the integer to the enum
class Enum2Bits a where
  -- toBits :: [a] -> Word32
  shiftL :: a -> Word32

instance Enum2Bits TcpState where
  -- toBits = enumsToWord
  shiftL state = B.shiftL 1 (fromEnum state)

instance Enum2Bits IDiagExt where
  -- toBits = enumsToWord
  shiftL state = B.shiftL 1 ((fromEnum state) - 1)

-- instance Enum2Bits INetDiag where
--   toBits = enumsToWord


enumsToWord :: Enum2Bits a => [a] -> Word32
enumsToWord [] = 0
enumsToWord (x:xs) = (shiftL x) .|. (enumsToWord xs)

-- TODO use bitset package ? but broken on nixos
wordToEnums :: Enum2Bits a =>  Word32 -> [a]
wordToEnums  _ = []

-- defined in include/uapi/linux/inet_diag.h
-- data InetDiagReq = InetDiagMsg {

-- {| This generates a response of inet_diag_msg
-- rename to answer ? |}
data InetDiagMsg = InetDiagMsg {
  idiag_family :: Word8
  , idiag_state :: Word8
  , idiag_timer :: Word8
  , idiag_retrans :: Word8
  , idiag_sockid :: InetDiagSockId
  , idiag_expires :: Word32
  , idiag_rqueue :: Word32
  , idiag_wqueue :: Word32
  , idiag_uid :: Word32
  , idiag_inode :: Word32
} deriving (Eq, Show)

-- see https://stackoverflow.com/questions/8633470/illegal-instance-declaration-when-declaring-instance-of-isstring
{-# LANGUAGE FlexibleInstances #-}

-- TODO this generates the  error "Orphan instance: instance Convertable [TcpState]"
-- instance Convertable [TcpState] where
--   getPut = putStates
--   getGet _ = return []

putStates :: [TcpState] -> Put
putStates states = putWord32host $ enumsToWord states


instance Convertable InetDiagMsg where
  getPut = putInetDiagMsg
  getGet _ = getInetDiagMsg

-- TODO rename to a TCP one ? SockDiagRequest
data SockDiagRequest = SockDiagRequest {
  sdiag_family :: Word8 -- ^AF_INET6 or AF_INET (TODO rename)
-- It should be set to the appropriate IPPROTO_* constant for AF_INET and AF_INET6, and to 0 otherwise.
  , sdiag_protocol :: Word8 -- ^IPPROTO_XXX always TCP ?
  -- IPv4/v6 specific structure
  , idiag_ext :: [IDiagExt] -- ^query extended info (word8 size)
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
      (wordToEnums extended :: [IDiagExt]) (wordToEnums states :: [TcpState])  _sockid

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

-- | 
-- Usually accompanied with attributes ?
getInetDiagMsg :: Get InetDiagMsg
getInetDiagMsg  = do
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
    return$  InetDiagMsg family state timer retrans _sockid expires rqueue wqueue uid inode

putInetDiagMsg :: InetDiagMsg -> Put
putInetDiagMsg msg = do
  putWord8 $ idiag_family msg
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
-- getWord32host
    sport <- getWord16host
    dport <- getWord16host
    -- iterate/ grow
    _src <- getByteString (4*4)
    _dst <- getByteString (4*4)
    _intf <- getWord32host
    cookie <- getWord64host
    return $ InetDiagSockId sport dport _src _dst _intf cookie

putInetDiagSockid :: InetDiagSockId -> Put
putInetDiagSockid cust = do
  -- we might need to clean up this a bit
  putWord16be $ idiag_sport cust
  putWord16be $ idiag_dport cust
  putByteString (idiag_src cust)
  putByteString (idiag_dst cust)
  -- putIPAddress (src cust)
  -- putIPAddress (dst cust)
  putWord32host $ idiag_intf cust
  putWord64host $ idiag_cookie cust

-- struct tcpvegas_info {
-- 	__u32	tcpv_enabled;
-- 	__u32	tcpv_rttcnt;
-- 	__u32	tcpv_rtt;
-- 	__u32	tcpv_minrtt;
-- };
-- data DiagVegasInfo = TcpVegasInfo {
--   -- TODO hide ?
--   tcpInfoVegasEnabled :: Word32
--   , tcpInfoRttCount :: Word32
--   , tcpInfoRtt :: Word32
--   , tcpInfoMinrtt :: Word32
-- }

-- instance Convertable DiagVegasInfo where
--   getPut  = putDiagVegasInfo
--   getGet _  = getDiagVegasInfo


-- putDiagVegasInfo ::  -> Put
-- putDiagVegasInfo info = error "should not be needed"


getDiagVegasInfo :: Get IDiagExtension
getDiagVegasInfo =
  TcpVegasInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6


-- getTcpInfo :: Get IDiagExtension
-- getTcpInfo =
--   DiagTcpInfo <$> getWord8
--   Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32

-- TODO generate with c2hsc ?
-- include/uapi/linux/inet_diag.h
data IDiagExtension =  DiagTcpInfo {
  tcpi_state :: Word8,
  tcpi_ca_state :: Word8,
  tcpi_retransmits :: Word8,
  tcpi_probes :: Word8,
  tcpi_backoff :: Word8,
  tcpi_options :: Word8,
  tcpi_wscales :: Word8,
  -- tcpi_snd_wscale : 4, tcpi_rcv_wscale : 4 :: Word8,

  tcpi_rto :: Word32,
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
  -- Not remembered, sorr
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
  tcpi_rcv_space :: Word32,
  tcpi_total_retrans :: Word32

} | Meminfo {
  idiag_rmem :: Word32
, idiag_wmem :: Word32
, idiag_fmem :: Word32
, idiag_tmem :: Word32
} | TcpVegasInfo {
-- tcpvegas_info 
  -- TODO hide ?
  tcpInfoVegasEnabled :: Word32
  , tcpInfoRttCount :: Word32
  , tcpInfoRtt :: Word32
  , tcpInfoMinrtt :: Word32
} | CongInfo String deriving (Show, Generic)

-- ideally we should be able to , Serialize
-- encode
-- instance Convertable IDiagExtension where
--   getGet _ = get
--   getPut = put

-- not sure what it is
-- INET_DIAG_MARK,		/* only with CAP_NET_ADMIN */

getTcpVegasInfo :: Get IDiagExtension
getTcpVegasInfo = TcpVegasInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

getMemInfo :: Get IDiagExtension
getMemInfo = Meminfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host


-- |
getCongInfo :: Get IDiagExtension
getCongInfo = do
    -- bytes = getListOf getWord8
    left <- remaining
    bs <- getByteString left
    return (CongInfo $ unpack bs)

-- Meminfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

getDiagTcpInfo :: Get IDiagExtension
getDiagTcpInfo =
   DiagTcpInfo <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  <*> getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host <*>getWord32host

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

showExtension :: IDiagExtension -> String
showExtension (CongInfo cc) = "Using CC " ++ (show cc)
showExtension (TcpVegasInfo _ _ rtt minRtt) = "RTT=" ++ (show rtt) ++ " minRTT=" ++ show minRtt
--   tcpi_state :: Word8,
showExtension (arg@DiagTcpInfo{}) = "TcpInfo: rtt/rttvar=" ++ show ( tcpi_rtt arg) ++ "/" ++ show ( tcpi_rttvar arg)
        ++ " snd_cwnd/ssthresh=" ++ show (tcpi_snd_cwnd arg) ++ "/" ++ show (tcpi_snd_ssthresh arg)
showExtension rest = show rest
-- "RTT=" ++ (show rtt) ++ " minRTT=" ++ show minRtt

-- | TODO use either ?
genQueryPacket :: (Either Word64 TcpConnection) -> [TcpState] -> [IDiagExt] -> Packet SockDiagRequest
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

  -- 1 => "lo". Check with ip link ?
  -- TODO pick from connection
  -- ifIndex = fromIntegral localhostIntfIdx :: Word32

  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo tcpStatesFilter diag_req
  in
    Packet hdr custom Map.empty

-- | to search for a specific connection
queryPacketFromCookie :: Word64 -> Packet SockDiagRequest
queryPacketFromCookie cookie =  genQueryPacket (Left cookie) [] []


loadExtension :: Int -> ByteString -> Maybe IDiagExtension
loadExtension key value = let
  fn = case toEnum key of
    -- MessageType shouldn't matter anyway ?!
    -- DiagCong error too few bytes
    InetDiagCong -> Just getCongInfo
    -- InetDiagNone -> Nothing
    InetDiagInfo -> Just getDiagTcpInfo
    -- InetDiagInfo ->  case runGet (getGet 42) value of
    --                     Right x -> Just x
    --                     _ -> Nothing
    InetDiagVegasinfo -> Just getTcpVegasInfo
    -- InetDiagTos -> Nothing
    -- InetDiagTclass -> Nothing
    -- InetDiagSkmeminfo -> Nothing
    -- InetDiagShutdown -> Nothing
    -- InetDiagDctcpinfo -> Nothing
    -- InetDiagProtocol -> Nothing
    -- InetDiagSkv6only -> Nothing
    -- InetDiagLocals -> Nothing
    -- InetDiagPeers -> Nothing
    -- InetDiagPad -> Nothing
    -- InetDiagMark -> Nothing
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
          Left err -> error $ "Decoding error " ++ err
