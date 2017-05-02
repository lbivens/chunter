.PHONY: deps rel package quick-test tree dist

APP=chunter

uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
uname_V6 := $(shell sh -c 'uname -v 2>/dev/null | cut -c-6 || echo not')


ifeq ($(uname_S),FreeBSD)
	PLATFORM = freebsd
endif
ifeq ($(uname_V6),joyent)
	PLATFORM = smartos
endif

include fifo.mk

# the kstat library will not compile on OS X

apps/chunter/priv/zonedoor: utils/zonedoor.c
# Only copile the zonedoor under sunus
	[ $(shell uname) != "SunOS" ] && cp `which cat` apps/chunter/priv/zonedoor || gcc -lzdoor utils/zonedoor.c -o apps/chunter/priv/zonedoor

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > chunter.version

version_header: version
	cp chunter.version rel/files/chunter.version
	@echo "-define(VERSION, <<\"$(shell cat chunter.version)\">>)." > apps/chunter/src/chunter_version.hrl

dist: ${PLATFORM} ;

freebsd: update ${PLATFORM}/rel
	gmake -C rel/pkgng package

smartos: update ${PLATFORM}/rel
	make -C rel/bootstrap bootstrap

clean:
	$(REBAR) clean
	make -C rel/bootstrap clean
	make -C rel/pkgng clean

freebsd/rel: version_header
	$(REBAR) as prod release

smartos/rel: apps/chunter/priv/zonedoor version_header
	-rm -r ./rel/chunter/share
	$(REBAR) as prod release
