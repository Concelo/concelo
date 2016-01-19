.PHONY: test
test: build/test.d

build/test.d: build/test-ignis.js
	nodejs $(<)
	touch $(@)

sources := $(shell find ./src ./test -name '*.hs' -o -name '*.js') 

build/test-ignis.js: $(sources)
	hastec \
		-isrc \
		-Wall \
		--onexec \
		--out=$(@) \
		--outdir=build \
		--with-js=src/ignis.js,test/test-ignis.js \
		'--start=$$HASTE_MAIN(); test();' \
		src/Main.hs
	mv src/Main.o src/Main.hi build

clean:
	rm -rf build
