let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190803/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190803/src/packages.dhall sha256:cf1acc1224e5b9fdd114fb085ee3ccc594622c2cdd25176c25dd9fdeff4f551c

in      upstream
    //  { bingsu =
            mkPackage
            [ "node-sqlite3", "typelevel-prelude" ]
            "https://github.com/justinwoo/purescript-bingsu.git"
            "v0.1.0"
        }
