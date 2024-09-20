export # export all variables to 'sub-Makefiles'

include test.cfg

test: | clean code actor wf-run check-output

check-output:
	@echo OUT_IWRAP_VERSION=$(shell iwrap --version) > expected.out
	@echo OUT_IMAS_VERSION=$(IMAS_VERSION) >> expected.out
	@echo OUT_IMAS_PREFIX=$(IMAS_PREFIX) >> expected.out
	@echo OUT_AL_VERSION=$(AL_VERSION) >> expected.out
	$(MAKE) -f ../Makefile.common $@

clean actor wf-run:
	$(MAKE) -f ../Makefile.common $@

code:
	@echo no code to be generated for dummy actor
