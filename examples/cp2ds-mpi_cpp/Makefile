.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 cp2ds-mpi_cpp_workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a core2dist_mpi_cpp -f ./cp2ds_mpi_cpp.yaml

actor-gui: native
	iwrap-gui -a core2dist_mpi_cpp -f ./cp2ds_mpi_cpp.yaml

native:
	make -C ./native_code clean
	make -C ./native_code

clean:
	@rm -rf wf_output.txt
	make -C ./native_code clean
