test : 
	cabal configure
	cabal build
	cp dist/build/test/test .

run : test
	./test

clean :
	rm -f -R dist
	rm test