build: deps
	npm run build

deps:
	npm install
	psc-package install

watch:
	parcel build/index.js
