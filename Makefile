BINARY = .gerbil/bin/jira
OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib

.PHONY: build clean

build:
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) $(BINARY)

clean:
	gerbil clean

install:
	sudo cp $(BINARY) /usr/local/bin
