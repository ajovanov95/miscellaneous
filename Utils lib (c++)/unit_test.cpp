#include "unit_test.hpp"

#include <map>
#include <string>
#include <functional>
#include <iostream>

using namespace std;

namespace utils {
	namespace tests {
		unit_test::unit_test(std::string name, bool break_on_failed) 
		{
			this->name = name;
			this->break_on_failed = break_on_failed;
		}
	
		unit_test::~unit_test() {}
		
		void unit_test::add_test(std::string name, test_method m) 
		{
			test_methods.insert (make_pair(name, m));
		}
		
		unsigned int unit_test::run() 
		{
			unsigned int passed = 0;
			unsigned int ran    = 0;
			unsigned int index  = 1;
	
			setup();
	
			cout << "Running tests for test class : \033[0;36m" << name << "\033[0m" << endl;
			cout << "\033[1;34m-------------------------------------------------\033[0m" << endl;
		
			for (auto p : test_methods) 
			{
				try 
				{
					const std::string text_passed = "\033[0;32mPASSED\033[0m";
					const std::string text_failed = "\033[0;31mFAILED\033[0m";
			
					cout << "(" << index << ")" << " Running test case \033[0;33m" << p.first << "\033[0m" << endl;
				
					bool test_passed = p.second();
				
					cout << "\tTest case " << p.first << " " << (test_passed ? text_passed : text_failed) << endl;
			
					ran++;
			
					if (test_passed) 
					{
						passed++;
					}
					else  
					{
						if (break_on_failed) 
						{
							break;
						}	
					}
				} 
				catch (exception ex) 
				{
					cout << "\tTest case " << p.first << " raised an exception" << endl;
					cout << "\tException info: \033[1;31m" << ex.what() << "\033[0m" << endl;
					break;  
				}
			
				index++;
			}
		
			cout << "\033[1;34m-------------------------------------------------\033[0m" << endl;
		
			if (passed == test_methods.size())
			{
				cout << "All test cases passed for \033[0;36m" << name << "\033[0m :)" << endl;
			}
			else 
			{
				cout << passed << " out of " << ran << " test cases ran passed for \033[0;36m" << name << "\033[0m "<< endl;
				cout << "The total number of test cases for this class is " << test_methods.size() << endl;
			}
		
			cleanup();
		
			return passed;
		}
	
		unsigned int unit_test::get_number_of_test_cases() 
		{
			return test_methods.size();
		}
	}
}
