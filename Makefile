PROJECT := jira

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-v $(PWD):/src:z \
	gerbil/alpine \
	make -C /src linux-static

linux-static: build
	/opt/gerbil/bin/gxc -o $(PROJECT)-bin -static \
	-cc-options "-Bstatic" \
	-ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lyaml -lz" \
	-exe $(PROJECT)/$(PROJECT).ss

clean:
	rm -rf $(PROJECT)-bin .gerbil

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)
