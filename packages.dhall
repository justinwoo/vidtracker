let upstream =
	  https://github.com/purescript/package-sets/releases/download/psc-0.13.2-20190803/packages.dhall sha256:cf1acc1224e5b9fdd114fb085ee3ccc594622c2cdd25176c25dd9fdeff4f551c

in      upstream
	//  { bingsu =
			{ dependencies = [ "node-sqlite3", "typelevel-prelude" ]
			, repo = "https://github.com/justinwoo/purescript-bingsu.git"
			, version = "v0.1.0"
			}
		, calpis =
			{ dependencies = [ "prelude" ]
			, repo = "https://github.com/justinwoo/purescript-calpis.git"
			, version = "v0.1.0"
			}
		, bonjiri =
			{ dependencies = [ "prelude" ]
			, repo = "https://github.com/justinwoo/purescript-bonjiri.git"
			, version = "v0.2.0"
			}
		}
