#include "test_maybe.hpp"

#include <cmath>
#include <iostream>

namespace utils {
	namespace tests {
		test_maybe::test_maybe() : unit_test ("Maybe", true) {}
	
		utils::maybe<double> test_maybe::mdivide (double a, double b) 
		{
			if (b != 0)
				return utils::just (a / b);
	
			return utils::nothing<double>();
		}

		utils::maybe<double> test_maybe::mlog (double num) 
		{
			if (num > 0)
				return utils::just (std::log (num));
		
			return utils::nothing<double>();
		}
	
		bool test_maybe::test_division_by_zero() 
		{
			utils::maybe<double> m = mdivide(10, 0);
			
			assert_equal (m.has_value(), false, "Bools are not ints. Justice for booleans.");
			
			return !m.has_value();
		}
		
		bool test_maybe::test_log_of_positive_number () 
		{
			utils::maybe<double> m = mlog(10);
			
			assert_equal (m.value(), std::log (10));
			
			return m.value() == std::log (10);
		}
		
		void test_maybe::setup() 
		{
			add_test ("division by zero", std::bind (&test_maybe::test_division_by_zero, this));
			add_test ("log of positive number", std::bind (&test_maybe::test_log_of_positive_number, this));
		}
		
		void test_maybe::cleanup() 
		{
		}
	}
}
