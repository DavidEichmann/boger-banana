default: clean main

main:
	# generate c code
	cgen -o cbits --header=Workarounds.h -i interface/Workarounds.if cpp/Workarounds.h
	# generate graph file
	grgen -o interface/Workarounds.gr -i interface/Workarounds.if cpp/Workarounds.h
	# generate haskell
	cgen-hs --inherit interface/Workarounds.gr --interface interface/Workarounds.if --hierarchy BB. -o BB cbits/*.h
	# copy files
	cp BB/*.hs ../BB
	# modify types to work with HOGRE and HOIS
	sed -i 's/newtype \(.*\)= [A-Z,a-z]*__\([a-z,A-Z,0-1]*\).*/type \1 = \2/g' ../BB/Types.hs
	sed -i '4i import Graphics.Ogre.Types\nimport OIS.Types\n' ../BB/Types.hs
	
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
