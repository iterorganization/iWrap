current_dir := $(shell pwd)
export CLASSPATH := $(CLASSPATH):$(current_dir)/jars/*
export CLASSPATH := $(CLASSPATH):$(current_dir)

all:
	mkdir -p build lib
	javac -cp $(CLASSPATH) ParametersUtils.java BasicTest.java -d build/
	jar cf lib/basic_test.jar  -C build/ .

clean:
	rm -rf ./build ./lib *.jar *~

