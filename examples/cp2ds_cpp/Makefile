.PHONY: native actor actor-gui wf-run


wf-run:
	python3 cp2ds_cpp_workflow.py

actor:
	iwrap -a core2dist_cpp -f ./cp2ds_cpp.yaml

actor-gui: native
	iwrap-gui -a core2dist_cpp -f ./cp2dsn_cpp.yaml

native:
	make -C ./native clean
	make -C ./native