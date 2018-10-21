    let mkPackage =
          https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/mkPackage.dhall

in  let upstream =
          https://raw.githubusercontent.com/justinwoo/spacchetti/211018/src/packages.dhall

in  let overrides =
          { halogen =
              upstream.halogen ⫽ { version = "master" }
          , halogen-vdom =
              upstream.halogen-vdom ⫽ { version = "v4.0.0" }
          }

in  upstream ⫽ overrides
