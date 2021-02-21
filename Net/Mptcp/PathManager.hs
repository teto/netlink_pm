{-
Trying to come up with a userspace abstraction for MPTCP path management

The default should deal

-- Need to deal with change in interface
-}
-- There should be
-- OnInterfaceChange


-- we should have a list of Interfaces as well
--class PathManager a where
--  -- when a new master connection is established
--  --
--  onMasterEstablishement MptcpConnection [NetworkInterface]

module Net.Mptcp.PathManager (
    PathManager (..)
    , NetworkInterface(..)
    , AvailablePaths
    , mapIPtoInterfaceIdx
    -- TODO don't export / move to its own file
    , handleAddr
    , globalInterfaces
) where

import Prelude hiding (concat, init)

import Net.Mptcp
import Net.IP
import Data.Word (Word32)
import qualified Data.Map as Map
import qualified System.Linux.Netlink.Route as NLR
import System.Linux.Netlink as NL
import Debug.Trace
import Control.Concurrent
-- import System.Linux.Netlink.Constants (eRTM_NEWADDR)
import System.Linux.Netlink.Constants as NLC
-- import qualified System.Linux.Netlink.Simple as NLS
import Data.ByteString (ByteString, empty)
import Data.ByteString.Char8 (unpack, init)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe
import Net.IPAddress

{-# NOINLINE globalInterfaces #-}
globalInterfaces :: MVar AvailablePaths
globalInterfaces = unsafePerformIO newEmptyMVar


interfacesToIgnore :: [String]
interfacesToIgnore = [
  "virbr0"
  , "virbr1"
  , "nlmon0"
  , "ppp0"
  , "lo"
  ]

-- basically a retranscription of NLR.NAddrMsg
-- TODO add flags ?
data NetworkInterface = NetworkInterface {
  ipAddress :: IP,   -- ^ Should be a list or a set
  interfaceName :: String,  -- ^ eth0 / ppp0
  interfaceId :: Word32  -- ^ refers to addrInterfaceIndex
} deriving Show



-- [NetworkInterface]
type AvailablePaths = Map.Map IP NetworkInterface



-- |
mapIPtoInterfaceIdx :: AvailablePaths -> IP -> Maybe Word32
mapIPtoInterfaceIdx paths ip =
    interfaceId <$> Map.lookup ip paths

-- class AvailableIPsContainer a where


-- |Reimplements
-- TODO we should not need the socket
-- onMasterEstablishement 
data PathManager = PathManager {
  name :: String
    -- interfacesToIgnore :: [String]
  , onMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
  -- , onAddrChange 

  -- should list advertised IPs
}

-- } deriving PathManager


-- TODO we should use the
handleInterfaceNotification
  :: AddressFamily -> Attributes -> Word32 -> Maybe NetworkInterface
handleInterfaceNotification addrFamily attrs addrIntf =

  -- case of
  --   Nothing -> Nothing
  --   Just val -> 
  -- TODO
  -- filter on flags too (UP), should be != LOOPBACK
  -- lo: <LOOPBACK,UP,LOWER_UP> and
  -- eno1: <BROADCAST,MULTICAST,UP,LOWER_UP
  case ifNameM of
    Nothing -> Nothing
    Just ifName -> case (elem ifName interfacesToIgnore ) of
                        True -> Nothing
                        False -> Just $ NetworkInterface ip ifName addrIntf
  where
    -- ip = undefined
    -- gets the bytestring / assume it always work
  ipBstr = fromMaybe empty (NLR.getIFAddr attrs)
  ifNameBstr = (Map.lookup NLC.eIFLA_IFNAME attrs)
  ifNameM = getString <$> ifNameBstr
  -- ip = getIPFromByteString addrFamily ipBstr
  ip = case (getIPFromByteString addrFamily ipBstr) of
    Right val -> val
    Left err -> undefined

-- taken from netlink
getString :: ByteString -> String
getString b = unpack (init b)


-- TODO handle remove/new event move to PathManager
-- todo should be pure and let daemon
handleAddr :: Either String NLR.RoutePacket -> IO ()
handleAddr (Left errStr) = putStrLn $ "Error decoding packet: " ++ errStr
handleAddr (Right (DoneMsg hdr)) =
  putStrLn $ "Error decoding packet: " ++ show hdr
handleAddr (Right (ErrorMsg hdr errorInt errorBstr)) =
  putStrLn $ "Error decoding packet: " ++ show hdr
-- TODO need handleMessage pkt
-- family maskLen flags scope addrIntf
handleAddr (Right (Packet hdr pkt attrs)) = do
  (putStrLn $ "received packet" ++ show pkt)
  oldIntfs <- trace "taking MVAR" (takeMVar globalInterfaces)

  let toto = (case pkt of
        arg@NLR.NAddrMsg{} ->
          let resIntf = handleInterfaceNotification (NLR.addrFamily arg) attrs (NLR.addrInterfaceIndex arg)
          in case resIntf of
                Nothing -> oldIntfs
                Just newIntf -> let
                  ip = ipAddress newIntf
                  in if msgType == eRTM_NEWADDR
                        then trace "adding ip" (Map.insert ip newIntf oldIntfs)
                        -- >> putStrLn "Added interface"
                        else if msgType == eRTM_GETADDR
                        then trace "GET_ADDR" oldIntfs

                        else if msgType == eRTM_DELADDR
                        then
                        trace "deleting ip" (Map.delete ip oldIntfs)
                        -- >> putStrLn "Removed interface"
                        else trace "other type" oldIntfs

        -- _ -> error "can't be anything else"
        arg@NLR.NNeighMsg{} -> trace "neighbor msg" oldIntfs
        arg@NLR.NLinkMsg{} -> trace "link msg" oldIntfs
        )

  trace ("putting mvar") (putMVar globalInterfaces $! (toto))

 where
    -- gets the bytestring
    msgType = messageType hdr

-- (arg@DiagTcpInfo{})


---- Updates the list of interfaces
---- should run in background
----
--trackSystemInterfaces :: IO()
--trackSystemInterfaces = do
--  -- check routing information
--  routingSock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
--  let cb = NLS.NLCallback (pure ()) (handleAddr . runGet getGenPacket)
--  NLS.nlPostMessage routingSock queryAddrs cb
--  NLS.nlWaitCurrent routingSock
--  dumpSystemInterfaces



-- fullmesh / ndiffports
    -- []

  -- where
  --   -- genPkt NetworkInterface
  --   -- let newSfPkt = newSubflowPkt mptcpSock newSubflowAttrs
  --   newSubflowAttrs = [
  --         MptcpAttrToken $ connectionToken con
  --       ]
  -- ++ (subflowAttrs $ masterSf { srcPort = 0 })
