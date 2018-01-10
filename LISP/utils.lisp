
(defun range (a b s)
	(cond
		((> a b) nil)
		((= a b) (cons b nil))
		(t (cons a (range (+ a s) b s)))
	)
)

(defun foldr (f s lst)
	"
		Fold to the right. E.g. foldr '+ 0 (1 2 3) gives (add 3 (add 2 (add 1 0)))
		f = function
		s = starting value
		lst = the list
	"
	(cond
		((null (cdr lst)) (funcall f (car lst) s)) ; kraj na rekurzijata, ostana uste samo eden element
		(t
			(funcall f
					(car lst)
					(foldr f s (cdr lst))
			)
		)
	)
)

(defun rev (lst)
	(foldr (lambda (x acc) (append acc (list x))) nil lst)
)

(defun prod (lst)
	(foldr '* 1 lst)
)

(defun fact (n)
	(prod (range 1 n 1))
)


(defun flatten (lst)
	(let*
		(
			(x (first lst))
			(rst (rest lst))
		)
		; action
		(cond
			((null x) nil)
			((listp x) (concat (flatten x) (flatten rst)))
			((atom x) (cons x (flatten rst)))
		)
	)
)

(defun cmap (f lst)
	(cond
		((null lst) nil)
		(t (cons
				(funcall f (car lst))
				(cmap f (cdr lst))
			)
		)
	)
)

(defun cfilter (p lst)
	(let
		(
			(f (car lst))
			(r (cdr lst))
		)

		(cond
			((null lst) nil)
			((eql (funcall p f) t) (cons f (cfilter p r)))
			(t (cfilter p r))
		)
	)
)
