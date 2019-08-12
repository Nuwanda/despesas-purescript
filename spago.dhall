{ name =
    "despesas-purescript"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "concur-react"
    , "format"
    , "numbers"
    , "debug"
    , "read"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
