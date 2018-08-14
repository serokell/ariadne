{ configuration-options =
    { configuration-file = ["cardano-configuration.yaml"] : Optional Text
    , configuration-key = ["mainnet_full"] : Optional Text
    , configuration-seed = [] : Optional Integer
    , system-start = [] : Optional Natural
    }
, log-config = [] : Optional Text
, log-prefix = ["@DATA/logs/mainnet"] : Optional Text
, rebuild-db = False
, db-path = ["@DATA/db-mainnet"] : Optional Text
, ekg-params = [] : Optional { IP : Text, PORT : Natural }
, keyfile = ["@DATA/secret-mainnet.key"] : Optional Text
, metrics = False
, default-port = [+3000] : Optional Natural
, node-id = ["node0"] : Optional Text
, topology = [] : Optional Text
}
