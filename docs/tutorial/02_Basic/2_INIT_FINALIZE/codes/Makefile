all: actor_cplus actor_fortran

iWrapped_code_fortran:
	make -C iWrapped_codes/code1_fortran all

iWrapped_code_cplus:
	make -C iWrapped_codes/code2_cpp all

actor_fortran: iWrapped_code_fortran
	iwrap --actor-type python --actor-name actor1_fortran --file actor1_fortran.yaml

actor_cplus: iWrapped_code_cplus
	iwrap --actor-type python --actor-name actor2_cpp --file actor2_cpp.yaml

wf-run:
	python workflow.py

clean:
	rm -f *~
	make -C iWrapped_codes/code1_fortran clean
	make -C iWrapped_codes/code2_cpp clean