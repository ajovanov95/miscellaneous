#include "unit_test.hpp"
#include "test_maybe.hpp"
#include "test_either.hpp"

#include <list>
#include <memory>
#include <iostream>

using namespace std;
using namespace utils::tests;

int main(int argc, char** argv) 
{
	list<unit_test*> tests = { 
		new test_maybe(),
		new test_either()
	};

	for (auto t : tests) 
	{	
		t->run();
		
		cout << endl;
		
		delete t;
	}
	
	return 0;
}
