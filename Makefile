PROJECT := jira
ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
PWD := $(shell pwd)
UID := $(shell id -u)
GID := $(shell id -g)

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build  $(PROJECT)

linux-static-docker: clean
	docker run -t \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil manifest.ss

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
