build: npm-deps purs-deps
	pp2n build
	make build-fe

npm-deps:
	npm install

purs-deps:
	pp2n install -j 100

build-fe:
	parcel build index.html
