namespace utils 
{
	template<typename T>
	maybe<T>::maybe() : is_nothing_(true) {}

	template<typename T>
	maybe<T>::maybe(maybe<T>&& m) : 
		is_nothing_(std::move(m.is_nothing_)), 
		value_(std::move (m.value_))
	{}

	template<typename T>
	bool maybe<T>::has_value() const
	{
		return !is_nothing_;
	}

	template<typename T>
	void maybe<T>::set_value(T t) 
	{
		value_ = t;

		is_nothing_ = false;
	}

	template<typename T>
	T maybe<T>::value() const
	{
		return value_;
	}

	template<typename T>
	void maybe<T>::nullify() 
	{
		is_nothing_ = true;
	}
	
	template<typename T>
	maybe<T>::operator bool() const
	{
		return has_value();
	}
	
	/**************************************/

	template<typename T>
	maybe<T> just(T t) 
	{
		maybe<T> m;
		m.set_value(t);
		return m;
	}

	template<typename T> 
	maybe<T> nothing() 
	{
		maybe<T> m;
		m.nullify();
		return m;
	}
}
