.PHONY: native actor actor-gui wf-run


wf-run:
	python3 cp2ds_workflow.py

actor:
	iwrap -a core2dist -f ./cp2ds.yaml

actor-gui: native
	iwrap-gui -a core2dist -f ./cp2ds.yaml

native:
	make -C ./native clean
	make -C ./native