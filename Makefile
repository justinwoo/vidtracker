build: npm-deps purs-deps
	npm run build

npm-deps:
	npm install

purs-deps:
	dhall format --inplace packages.dhall
	spago psc-package-insdhall
	psc-package2nix
	./install-deps.nix

watch:
	parcel build/index.js

build-fe:
	psc-package build -- -g corefn
	zephyr -f FrontEnd.main
	purs bundle "dce-output/*/*.js" -m FrontEnd -o output/build/index.js
	parcel build output/build/index.js -d dist
