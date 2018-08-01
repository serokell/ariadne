{ common-args =
    { configuration-options =
        { configuration-file = ["./cardano/cardano-config.yaml"] : Optional Text
        }
    , log-config = ["./cardano/log-config.yaml"] : Optional Text
    , log-prefix = ["@DATA/logs/mainnet"] : Optional Text
    }
, rebuild-db = False
, db-path = ["@DATA/db-mainnet"] : Optional Text
, dump-configuration = False
, dump-genesis-data-to = [] : Optional Text
, ekg-params = [] : Optional { IP : Text, PORT : Natural }
, genesis-secret = [] : Optional Integer
, keyfile = ["@DATA/secret-mainnet.key"] : Optional Text
, metrics = False
, network-config =
    { default-port = [+3000] : Optional Natural
    , topology = ["./cardano/topology.yaml"] : Optional Text
    }
, route53-health-check = [] : Optional { IP : Text, PORT : Natural }
, statsd-params = [] : Optional { statsd-debug : Optional Bool, statsd-interval : Optional Integer, statsd-prefix : Optional Text, statsd-server : { IP : Text, PORT : Natural }, statsd-suffix : Optional Text }
, update-latest-path = [] : Optional Text
, update-with-package = False
}
