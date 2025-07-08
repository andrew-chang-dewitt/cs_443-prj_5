main:
	dune build
	cp _build/default/src/main.exe main
	chmod 771 main

clean:
	rm -rf main
	dune clean

test: main
	./test.sh p5tests
	@printf "\n\n"

perf: main
	time --format="user\t%U\nsystem\t%S\nelapsed\t%E\n" ./main -nossa -interpllvm ./proj5_tests/simple.ll -o ./proj5_tests/simple
	@printf "\n\n"
