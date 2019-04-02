.POSIX:
PREFIX = /usr/local

.SUFFIXES:
all:
	chicken-install srfi-1
	chicken-install srfi-69
	chicken-install typed-records
	csc -verbose -strict-types -O3 -lfa2 zebrah.scm -o zebrah
install:
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/share/man/man1
	cp zebrah $(PREFIX)/bin
	cp zebrah.1 $(PREFIX)/share/man/man1
uninstall:
	rm $(PREFIX)/bin/zebrah
	rm $(PREFIX)/share/man/man1/zebrah.1
	$(info )
	$(info Chicken packages srfi-1 srfi-69 and typed-records may have been installed by this makefile, but they were not automatically uninstalled. Please run chicken-uninstall <package name> if you'd like to remove any of them.)
	$(info )
