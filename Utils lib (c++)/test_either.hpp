#ifndef TEST_EITHER_HPP
#define TEST_EITHER_HPP

#include "unit_test.hpp"

namespace utils 
{
	namespace tests 
	{
		class test_either : public unit_test 
		{
			public:
				test_either();
			
				bool test_division_by_zero ();
			
				void setup();
				
				void cleanup();
		};
	}
}

#endif
