default: clean main

main: lib-workarounds
	# generate graph file
	grgen -o interface/Workarounds.gr -i interface/Workarounds.if cpp/Workarounds.h
	# generate haskell
	cgen-hs --inherit interface/Workarounds.gr --interface interface/Workarounds.if --hierarchy BB. -o BB cbits/*.h
	# build the test haskel main
	#ghc --make -o main -L. -lworkarounds -lstdc++ hs/main.hs
	cp BB/*.hs ../BB
	cp libworkarounds.a ../../lib
	
	

lib-workarounds:
	# generate c
	cgen -o cbits --header=Workarounds.h -i interface/Workarounds.if cpp/Workarounds.h
	# make a library
	g++ -c -Icpp -o cbits/Workarounds.o cbits/Workarounds.cpp
	ar q libworkarounds.a cbits/Workarounds.o
	#
	# Now u can put libworkarounds.a into a directory /some/path and then compile with ghc ... -L/some/path -lworkarounds
	# NOTE that in the -l option u remove the "lib" and ".a" pre and postfix
	#

clean:
	rm -f -r 'cbits'
	rm -f -r 'BB'
	rm -f -r 'interface/Dfn.gr'
	rm -f -r *.o
	rm -f -r *.hi
	rm -f -r hs/*.o
	rm -f -r hs/*.hi
	rm -f -r *.a
	rm -f -r *.hs
	rm -f main
