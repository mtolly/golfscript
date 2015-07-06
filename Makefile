# Builds win/mac/linux binaries from a Mac with Vagrant.

release := $(shell release/version)

.PHONY: all osx linux win32
all: osx linux win32
osx: release/${release}-osx-x64.zip
linux: release/${release}-linux-x64.tar.gz
win32: release/${release}-win32-x86.zip

release/${release}-osx-x64.zip:
	stack setup
	stack build
	cp .stack-work/install/x86_64-osx/*/*/bin/golfscript golfscript
	strip golfscript
	zip $@ golfscript README.md
	rm golfscript

release/${release}-linux-x64.tar.gz:
	vagrant up linux
	vagrant ssh linux -c "cd /vagrant && stack setup && stack build"
	cp .stack-work/install/x86_64-linux/*/*/bin/golfscript golfscript
	vagrant ssh linux -c "cd /vagrant && strip golfscript"
	tar -cvzf $@ golfscript README.md
	rm golfscript

release/${release}-win32-x86.zip:
	vagrant up wine
	vagrant ssh wine -c "cd /vagrant && wine stack setup && wine stack build"
	cp .stack-work/install/i386-windows/*/*/bin/golfscript.exe golfscript.exe
	zip $@ golfscript.exe README.md
	rm golfscript.exe
