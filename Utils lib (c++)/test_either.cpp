#include "either.hpp"
#include "test_either.hpp"

#include <functional>
#include <string>
#include <exception>

using namespace std;

namespace utils 
{
	namespace tests 
	{
		test_either::test_either() : unit_test("Either", true)
		{}
		
		bool test_either::test_division_by_zero() 
		{
			double a = 20;
			double b = 0;
			
			either<double, string> result;
			
			if (b == 0) 
			{
				result = utils::right<double, string>("ZeroDivisionError");
			}
			else 
			{
				result = utils::left<double, string>(a / b);
			}
			
			assert_equal(result.is_left(), false, "Result's left value is not active");
			assert_equal(result.is_right(), true, "Result's left value is active");
			assert_equal(result.right(), string("ZeroDivisionError"));
			
			return result.is_right();
		}
		
		void test_either::setup () 
		{
			add_test ("division by zero either", std::bind (&test_either::test_division_by_zero, this));
		}
		
		void test_either::cleanup() 
		{
		
		}
	}
}
