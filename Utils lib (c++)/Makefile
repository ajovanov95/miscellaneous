FLGS = -g -Wall -std=c++11
EXE  = tests
SRCS = unit_test.cpp test_maybe.cpp test_either.cpp tests_main.cpp

all:
	g++ $(FLGS) $(SRCS) -o $(EXE)

run-tests:
	./$(EXE)

clean:
	rm -f *~
	rm -f $(EXE)
