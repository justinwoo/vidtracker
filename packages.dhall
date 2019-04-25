let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190423/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190423/src/packages.dhall sha256:d5061e7c3e710034c73e33d9b4e7e0cec98d9a8f161802a2186c646f012f2e1f

in    upstream
    ⫽ { simple-json =
          upstream.simple-json ⫽ { version = "v5.1.0" }
      , motsunabe =
          mkPackage
          [ "lists", "strings" ]
          "https://github.com/justinwoo/purescript-motsunabe.git"
          "v1.0.0"
      , simple-json-utils =
          mkPackage
          [ "motsunabe", "simple-json" ]
          "https://github.com/justinwoo/purescript-simple-json-utils.git"
          "v0.1.0"
      , tortellini =
          upstream.tortellini ⫽ { version = "v5.1.0" }
      }
