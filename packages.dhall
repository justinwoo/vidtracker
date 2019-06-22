let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190611/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190611/src/packages.dhall sha256:2631fe1dce429e5f27324e59cc834e8e8832a5d533423952105c7446320a3648

in      upstream
    //  { bingsu =
            mkPackage
            [ "node-sqlite3", "typelevel-prelude" ]
            "https://github.com/justinwoo/purescript-bingsu.git"
            "v0.1.0"
        }
