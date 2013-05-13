module Main where

import Network.Metricsd.Client

main = do
    mcHandle <- initMetricsdClient
    sendMeter mcHandle "haskelltestrun"
