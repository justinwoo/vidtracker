build: npm-deps purs-deps
	spago2nix build
	make build-fe

npm-deps:
	npm install

purs-deps:
	spago2nix install -j 100

build-fe:
	parcel build index.html
