{ name =
    "despesas-purescript"
, dependencies =
    [ "aff"
    , "concur-react"
    , "console"
    , "debug"
    , "effect"
    , "format"
    , "indexedDB"
    , "numbers"
    , "parallel"
    , "psci-support"
    , "read"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
