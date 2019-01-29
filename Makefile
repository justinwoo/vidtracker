build: npm-deps purs-deps
	npm run build

npm-deps:
	npm install

insdhall:
	dhall format --inplace packages.dhall
	spago psc-package-insdhall
	psc-package2nix

purs-deps:
	pp2n install

watch:
	parcel build/index.js

build-fe:
	pp2n build -g corefn
	zephyr -f FrontEnd.main
	purs bundle "dce-output/*/*.js" -m FrontEnd -o fe-bundle.js
	parcel build fe-bundle.js -d dist
