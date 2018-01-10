#ifndef EITHER_HPP
#define EITHER_HPP

#include <utility>

namespace utils {
	const bool EITHER_SIDE_LEFT  = true;
	const bool EITHER_SIDE_RIGHT = false;

	template <typename Left, typename Right>
	class either {
		private:
			Left l;
			Right r;
			
			bool side;
			
		public:
			either();
			either(either<Left, Right>&&);
		
			either<Left, Right>& operator=(const either<Left,Right>&);
			
			Left left() const;
			Right right() const;
	
			bool is_left() const;
			bool is_right() const;
		
			void set_left(Left l);
			void set_right(Right r);
	};
	
	template <typename Left, typename Right>
	either<Left, Right> left(Left l);
	
	template <typename Left, typename Right>
	either<Left, Right> right(Right r);
}

#include "either.cpp"

#endif
