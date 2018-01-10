#ifndef MAYBE_HPP
#define MAYBE_HPP

#include <utility>

namespace utils {

	template<typename T>
	class maybe {
		private:
			bool is_nothing_;
			T value_;
	
		public:
			maybe();
		
			maybe(maybe<T>&&);
		
			bool has_value() const;

			T value() const;

			void set_value(T t);

			void nullify();
			
			explicit operator bool() const;
	};

	template<typename T> maybe<T> just(T t);
	template<typename T> maybe<T> nothing();
}

#include "maybe.cpp"

#endif
