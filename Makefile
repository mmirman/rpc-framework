test : 
	cabal configure
	cabal build
	cp dist/build/rpc-test/rpc-test .

run : test
	./rpc-test

clean :
	rm -f -R dist
	rm rpc-test