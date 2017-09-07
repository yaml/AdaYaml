all: adayaml

adayaml:
	gprbuild -p -s -P yaml.gpr

test:
	gprbuild -p -s -P yaml-tests.gpr

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
