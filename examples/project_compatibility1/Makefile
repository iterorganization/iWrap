test: output/updated_1_v0.6.0.yaml \
      output/updated_1_v0.7.0.yaml \
      output/updated_1_v0.10.0.yaml \
      output/updated_1_newest.yaml

IMAS_VERSION=3.37.0
output/updated_%.yaml: input/test_%.yaml
	rm -f output/*
	mkdir -p output
	iwrap-yaml-update  -f $< -o $@
	@cmp expected_output.yaml $@ &> /dev/null || \
	   (echo 'ERROR: expected and real output differs!' && exit 1 )

clean:
	rm -f output/*
