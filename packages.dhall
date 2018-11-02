    let mkPackage =
          https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/mkPackage.dhall sha256:806af8ce2510ded25a8aa9565e26852458e5dc83ddd3e8e39b04441392648f90

in  let upstream =
          https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/packages.dhall sha256:803c6b12188bbe1b058534545bbbb251cde06a47dbdae8a6eba539b8c4937730

in  let overrides =
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
