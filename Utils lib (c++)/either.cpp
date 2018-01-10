namespace utils {
	template <typename Left, typename Right>
	either<Left,Right>::either() {}

	template <typename Left, typename Right>
	either<Left, Right>::either(either<Left, Right>&& e) : 
		l(std::move (e.l)),
		r(std::move (e.r)),
		side(std::move (e.side))
	{}
	
	template <typename Left, typename Right>
	either<Left, Right>& either<Left, Right>::operator=(const either<Left,Right>& rhs) 
	{
		this->side = rhs.side;
		
		if (this->side == EITHER_SIDE_LEFT)
			this->l = rhs.l;
		else
			this->r = rhs.r;
			
		return *this;
	}
	
	template <typename Left, typename Right>		
	Left either<Left, Right>::left() const
	{
		return l;
	}

	template <typename Left, typename Right>
	Right either<Left, Right>::right() const
	{
		return r;
	}

	template <typename Left, typename Right>
	bool either<Left, Right>::is_left() const
	{
		return side == EITHER_SIDE_LEFT;
	}

	template <typename Left, typename Right>
	bool either<Left, Right>::is_right() const
	{
		return side == EITHER_SIDE_RIGHT;
	}

	template <typename Left, typename Right>
	void either<Left, Right>::set_left(Left l) 
	{
		this->l = l;
		this->side = EITHER_SIDE_LEFT;
	}

	template <typename Left, typename Right>
	void either<Left, Right>::set_right(Right r) 
	{
		this->r = r;
		this->side = EITHER_SIDE_RIGHT;
	}
	
	template <typename Left, typename Right>
	either<Left, Right> left(Left l) 
	{
		either<Left, Right> e;
		e.set_left(l);
		return e;
	}
	
	template <typename Left, typename Right>
	either<Left, Right> right(Right r) 
	{
		either<Left, Right> e;
		e.set_right(r);
		return e;
	}
}
