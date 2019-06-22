{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "aff"
    , "bingsu"
    , "choco-pie"
    , "console"
    , "debug"
    , "effect"
    , "makkori"
    , "milkis"
    , "naporitan"
    , "node-fs-aff"
    , "node-process"
    , "node-sqlite3"
    , "psci-support"
    , "react-basic"
    , "simple-json"
    , "simple-json-utils"
    , "sunde"
    , "test-unit"
    , "toppokki"
    , "tortellini"
    ]
, packages =
    ./packages.dhall
}
