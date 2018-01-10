#include "maybe.hpp"
#include "unit_test.hpp"

namespace utils {
	namespace tests {
		class test_maybe : public unit_test {
			private:
				utils::maybe<double> mdivide (double a, double b);
				utils::maybe<double> mlog (double num);
				
			public:
				test_maybe();
				
				bool test_division_by_zero();
				bool test_log_of_positive_number();
				
				void setup();
				
				void cleanup();
		};
	}
}
