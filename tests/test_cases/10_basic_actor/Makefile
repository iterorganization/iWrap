export # export all variables to 'sub-Makefiles'

include test.cfg

test: | clean code actor wf-run check-output

clean code actor wf-run check-output:
	$(MAKE) -f ../Makefile.common $@
