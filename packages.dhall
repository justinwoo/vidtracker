let mkPackage =
      https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/mkPackage.dhall sha256:8e1c6636f8a089f972b21cde0cef4b33fa36a2e503ad4c77928aabf92d2d4ec9

let upstream =
      https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/packages.dhall sha256:d229157cc35d5c846bd6a96d9b9e34d2307c8da34d2d9532a03259356909c9d5

let overrides =
      { halogen =
            upstream.halogen
          ⫽ { repo =
                "https://github.com/justinwoo/purescript-halogen.git"
            , version =
                "v5.0.0-pre"
            }
      , halogen-vdom =
          upstream.halogen-vdom ⫽ { version = "v4.0.0" }
      }

in  upstream ⫽ overrides
