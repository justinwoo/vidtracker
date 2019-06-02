let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/packages.dhall sha256:5da1578dd297709265715a92eda5f42989dce92e121fcc889cff669a3b997c3d

in      upstream
    //  { simple-json =
            upstream.simple-json // { version = "v5.1.0" }
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
            upstream.tortellini // { version = "v5.1.0" }
        }
