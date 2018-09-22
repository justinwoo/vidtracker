build: deps
	npm run build

deps:
	npm install
	psc-package install

format-dhall:
	dhall format --inplace packages.dhall

insdhall:
	spacchetti insdhall

watch:
	parcel build/index.js
