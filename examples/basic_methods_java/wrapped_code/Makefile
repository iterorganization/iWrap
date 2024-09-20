all:
	mkdir -p build lib
	javac -cp $(CLASSPATH):java_dependencies/* src/*.java -d build/
	jar cf basic_test.jar  -C build/ .


clean:
	rm -rf build *.jar *~


