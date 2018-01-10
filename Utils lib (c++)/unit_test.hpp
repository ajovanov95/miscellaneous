#ifndef UNIT_TEST_HPP
#define UNIT_TEST_HPP

#include <map>
#include <string>
#include <functional>
#include <iostream>

using namespace std;

namespace utils {
	namespace tests {
		typedef function<bool()> test_method;

		class unit_test {
			public:
				unit_test(std::string name = "Generic test class", 
						 bool break_on_failed = false);
						 
				virtual ~unit_test();
		
				virtual void setup() = 0;
		
				void add_test(std::string name, test_method m);
		
				virtual void cleanup() = 0;
		
				unsigned int run();
			
				unsigned int get_number_of_test_cases();
			
			private:
				map<string, test_method> test_methods;
				std::string name;
				bool break_on_failed;
		};
	
		template<typename T>
		bool assert_equal(T received, T expected, 
						  std::string msg_passed = "",
						  std::string msg_failed = "") 
		{
			if (received == expected) 
			{
				cout << "\tAssertion passed (received "
					 << received << " which is equal to the expected " << expected << ")" << endl;
					 
				if (msg_passed != "")
					cout << "\tAssertion message: \"" << msg_passed << "\"" << endl;	 
					 
				return true;
			}
			else 
			{
				cout << "\tAssertion failed (received " << received 
					 << " but that is different from the expected " << expected << ")" << endl;
					 
				if (msg_failed != "")
					cout << "\tAssertion message : \"" << msg_failed << "\"" << endl;		 
				
				return false;
			}
		}
	}
}

#endif
