all: ext-rvhyper ext-leviathan ext-strix
	cargo build --release
	cp target/release/rehyper bin/

ext-rvhyper:
	make -C external/rvhyper
	mkdir -p bin
	cp external/rvhyper/build/release/rvhyper bin/

ext-leviathan:
	mkdir -p external/leviathan/build
	mkdir -p bin
	cd external/leviathan/build; ../configure
	make -C external/leviathan/build

ext-strix:
	make -C external/strix
	mkdir -p bin
	cp external/strix/bin/strix bin/
	cp external/strix/bin/owl.jar bin/

clean:
	rm -rf bin/
	cargo clean
	make -C external/rvhyper clean
	rm -rf external/leviathan/bin
	rm -rf external/leviathan/build
	make -C external/strix clean
