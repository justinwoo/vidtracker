{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "my-project"
, dependencies =
	[ "aff"
	, "bingsu"
	, "bonjiri"
	, "calpis"
	, "choco-pie"
	, "console"
	, "debug"
	, "effect"
	, "makkori"
	, "naporitan"
	, "node-fs-aff"
	, "node-process"
	, "node-sqlite3"
	, "psci-support"
	, "react-basic"
	, "simple-json"
	, "simple-json-utils"
	, "sunde"
	, "test-unit"
	, "toppokki"
	, "tortellini"
	]
, packages = ./packages.dhall
}
