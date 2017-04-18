.PHONY: deps rel package quick-test tree
APP=chunter

all: apps/chunter/priv/zonedoor version_header compile 

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

package: update rel
	make -C rel/pkg package

clean:
	$(REBAR) clean
	make -C rel/pkg clean

rel: apps/chunter/priv/zonedoor version_header
	-rm -r ./rel/chunter/share
	$(REBAR) as prod release
