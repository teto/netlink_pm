{-|
Module      : TCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
module Net.Tcp (
    module Net.Tcp.Definitions
    , module Net.Tcp.Constants
    -- , Net.Tcp.reverse
    -- , Net.Tcp.nameFromTcpConnection
    , tcpFlagRef
    , numberToTcpFlags
) where

import Net.Tcp.Definitions
import Net.Tcp.Constants

import Data.Bits ((.&.))

-- TODO these should be generated
tcpFlagRef :: TcpFlag -> Int
tcpFlagRef TcpFlagFin = 1
tcpFlagRef TcpFlagSyn = 2
tcpFlagRef TcpFlagAck = 8


numberToTcpFlags :: Int -> [TcpFlag]
numberToTcpFlags n = Prelude.filter  (\x -> combi x /= 0 ) list
    where
        combi x = (.&.) n (tcpFlagRef x)
        list = [TcpFlagSyn, TcpFlagAck, TcpFlagFin ]

