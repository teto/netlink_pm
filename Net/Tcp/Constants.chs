{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Net.Mptcp.Constants
Description : A module to bridge the haskell code to underlying C code

I consider this module internal.
The documentation may be a bit sparse.
Inspired by:
https://stackoverflow.com/questions/6689969/how-does-one-interface-with-a-c-enum-using-haskell-and-ffi

TODO might be best to just use the netlink script and adapt it
https://github.com/Ongy/netlink-hs/issues/7
-}
module Net.Tcp.Constants
where

import Data.Bits ()
import Data.Word (Word8, Word32)

-- copy from include/net/tcp_states since it's not part of the user API
#include <tcp_states.h>

-- For anonymous C enums, we can use , Bits
{#enum TCP_ESTABLISHED as TcpState {underscoreToCase} deriving (Eq, Show)#}

-- tcp_ca_state is a bitfield see
-- http://www.yonch.com/tech/linux-tcp-congestion-control-internals
#include <linux/tcp.h>


{#enum TCP_CA_Open as TcpCAState {underscoreToCase} deriving (Eq, Show)#}

-- GenBind.evalCCast: Casts are implemented only for integral constants
-- {#enum define TcpFlag {TCP_FLAG_SYN as TcpFlagSyn} deriving (Eq, Show)#}

data TcpFlag = TcpFlagSyn | TcpFlagFin | TcpFlagAck

-- {#const TCP_FLAG_SYN#}
tcpFlagSynVal :: Word32
tcpFlagSynVal = 2

tcpFlagFinVal :: Word32
tcpFlagFinVal = 1

tcpFlagAckVal :: Word32
tcpFlagAckVal = 8
-- tcpFlagAck :: Word8
-- tcpFlagAck = {#const TCP_FLAG_ACK #}


-- TCP_FLAG_CWR = __constant_cpu_to_be32(0x00800000),
-- TCP_FLAG_ECE = __constant_cpu_to_be32(0x00400000),
-- TCP_FLAG_URG = __constant_cpu_to_be32(0x00200000),
-- TCP_FLAG_ACK = __constant_cpu_to_be32(0x00100000),
-- TCP_FLAG_PSH = __constant_cpu_to_be32(0x00080000),
-- TCP_FLAG_RST = __constant_cpu_to_be32(0x00040000),
-- TCP_FLAG_SYN = __constant_cpu_to_be32(0x00020000),
-- TCP_FLAG_FIN = __constant_cpu_to_be32(0x00010000),

-- #define TH_FIN  0x0001
-- #define TH_SYN  0x0002
-- #define TH_RST  0x0004
-- #define TH_PUSH 0x0008
-- #define TH_ACK  0x0010

