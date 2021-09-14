.PHONY: clean docs

clean:
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

