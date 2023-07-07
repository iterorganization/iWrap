.PHONY: native actor actor-gui wf-run

test:
	iwrap-yaml-update  -f ./input/outdated.yaml -o updated.yaml
	@cmp expected_output.txt updated.yaml &> /dev/null || \
	(echo 'ERROR: expected and real output differs!' && exit 1 )