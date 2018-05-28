{ common-args = 
    { configuration-options = 
        { configuration-file = ["./cardano/cardano-config.yaml"] : Optional Text
        , configuration-key = ["mainnet_full"] : Optional Text
        , configuration-seed = [] : Optional Integer
        , system-start = [] : Optional Natural 
        }
    , log-config = ["./cardano/log-config.yaml"] : Optional Text
    , log-prefix = ["../logs/mainnet"] : Optional Text
    , report-servers = [] : List Text
    , update-servers = [] : List Text 
    }
, rebuild-db = False
, db-path = ["../db-mainnet"] : Optional Text
, dump-configuration = False
, dump-genesis-data-to = [] : Optional Text
, ekg-params = [] : Optional { IP : Text, PORT : Natural }
, genesis-secret = [] : Optional Integer
, json-log = [] : Optional Text
, keyfile = ["../secret-mainnet.key"] : Optional Text
, metrics = False
, network-config = 
    { address = [] : Optional { IP : Text, PORT : Natural }
    , default-port = [+3000] : Optional Natural
    , kademlia = [] : Optional Text
    , listen = [] : Optional { IP : Text, PORT : Natural }
    , node-id = ["node0"] : Optional Text
    , policies = [] : Optional Text
    , topology = ["./cardano/topology.yaml"] : Optional Text 
    }
, route53-health-check = [] : Optional { IP : Text, PORT : Natural }
, statsd-params = [] : Optional { statsd-debug : Optional Bool, statsd-interval : Optional Integer, statsd-prefix : Optional Text, statsd-server : { IP : Text, PORT : Natural }, statsd-suffix : Optional Text }
, update-latest-path = [] : Optional Text
, update-with-package = False
}
