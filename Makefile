all: adayaml

adayaml:
	gprbuild -p -s -P yaml.gpr

test:
	gprbuild -p -s -P yaml-tests.gpr

test-coverage:
	gprbuild -p -s -P yaml-tests.gpr --subdirs=cvg \
		-cargs -g -fdump-scos -fpreserve-control-flow
test-coverage-run: test-coverage
	rm -rf cvg
	mkdir cvg
	gnatcov run -o cvg/yaml-loading_tests-harness.trace \
		test/bin/cvg/yaml-loading_tests-harness
	gnatcov run -o cvg/yaml-dumping_tests-harness.trace \
		test/bin/cvg/yaml-dumping_tests-harness
	gnatcov coverage -P yaml-tests.gpr --projects yaml --subdirs=cvg \
		--annotate dhtml --level stmt+decision \
		--output-dir=cvg cvg/*.trace
	@echo "==="
	@echo "HTML coverage report generated at $$PWD/cvg/index.html"

utils:
	gprbuild -p -s -P yaml-utils.gpr

server:
	gprbuild -p -s -P yaml-server.gpr

dropin:
	gprbuild -p -s -P libyaml_dropin.gpr

dropin_utils:
	gprbuild -p -s -P libyaml_dropin-utils.gpr

clean:
	gprclean -P yaml.gpr
	gprclean -P yaml-tests.gpr
	gprclean -P yaml-utils.gpr
	gprclean -P yaml-server.gpr
	gprclean -P libyaml_dropin.gpr
	gprclean -P libyaml_dropin-utils.gpr

.PHONY: adayaml test utils all dropin dropin_utils
