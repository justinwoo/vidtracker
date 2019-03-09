build: npm-deps purs-deps
	npm run build

npm-deps:
	npm install

purs-deps:
	pp2n install

watch:
	parcel build/index.js

build-fe:
	pp2n build
	parcel build index.html
