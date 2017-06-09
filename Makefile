all: adayaml

adayaml:
	gprbuild -p -s -P yaml.gpr

test:
	gprbuild -p -s -P yaml_unittests.gpr

utils:
	gprbuild -p -s -P yaml_utils.gpr

clean:
	gprclean -P yaml.gpr
	gprclean -P yaml_unittests.gpr
	gprclean -P yaml_utils.gpr

.PHONY: adayaml test utils all
