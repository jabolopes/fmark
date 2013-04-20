all: dist/bin dist/obj
	ghc --make Main.hs -o dist/bin/fmark -outputdir dist/obj

dist/bin:
	mkdir -p dist/bin

dist/obj:
	mkdir -p dist/obj

clean:
	rm -rf dist
