# Checks for this Emacs config.  See "Checking the config" in README.org.
#
#   make check                 drift, then tests
#   make drift                 are the tangled files up to date with the Org files?
#   make test                  all ERT tests in tests/*-tests.el
#   make test SELECTOR=gjg-sql only tests whose names match a regexp

EMACS ?= emacs
SELECTOR ?= .

BATCH = $(EMACS) -Q --batch -L tests -l gjg-config-test
TESTS = $(wildcard tests/*-tests.el)

.PHONY: check drift test

# Run both even when drift fails (the tests read the Org files, not the
# tangled ones), then fail if either failed.
check:
	@status=0; $(MAKE) --no-print-directory drift || status=1; \
	$(MAKE) --no-print-directory test || status=1; exit $$status

drift:
	$(BATCH) -f gjg-config-test-drift-batch

test:
	$(BATCH) $(patsubst %,-l %,$(TESTS)) \
		--eval '(ert-run-tests-batch-and-exit "$(SELECTOR)")'
