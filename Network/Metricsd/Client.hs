module Network.Metricsd.Client
    ( initMetricsdClient
    , initMetricsdClient'
    , sendCounter
    , sendGauge
    , sendHistogram
    , sendMeter
    , MetricsdClientHandle
    ) where

import Control.Applicative
import Network.Socket

newtype MetricsdClientHandle = MetricsdClientHandle
                                { _unMCH :: (Socket, SockAddr) }

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: PortNumber
defaultPort = 8125

-- | Initialize metricsd client with default hostname and port (127.0.0.1:8125).
initMetricsdClient :: IO MetricsdClientHandle
initMetricsdClient = initMetricsdClient' defaultHost defaultPort

-- | Initialize metricsd client with the given hostname and port.
initMetricsdClient' :: String -> PortNumber -> IO MetricsdClientHandle
initMetricsdClient' host port = do
    s <- socket AF_INET Datagram defaultProtocol
    sockAddr <- SockAddrInet port <$> inet_addr host
    return $ MetricsdClientHandle (s, sockAddr)

sendCounter :: MetricsdClientHandle -> String -> Integer -> IO ()
sendCounter h name value = sendMetric h $ name ++ ":" ++ show value ++ "|c\n"

sendGauge :: MetricsdClientHandle -> String -> Integer -> IO ()
sendGauge h name value = sendMetric h $ name ++ ":" ++ show value ++ "|g\n"

sendHistogram :: MetricsdClientHandle -> String -> Integer -> IO ()
sendHistogram h name value = sendMetric h $ name ++ ":" ++ show value ++ "|h\n"

sendMeter :: MetricsdClientHandle -> String -> IO ()
sendMeter h name = sendMetric h name

sendMetric :: MetricsdClientHandle -> String -> IO ()
sendMetric (MetricsdClientHandle (s, sockAddr)) msg =
    sendTo s msg sockAddr >> return ()
