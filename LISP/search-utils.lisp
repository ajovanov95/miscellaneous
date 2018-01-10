
(defun find-parent (node preds)
	(cond
		((null preds) nil)

		((equal (car (car preds)) node) (cadr (car preds)))

		(t (find-parent node (cdr preds)))
	)
)

(defun backtrack-path (node preds)
	(let
		(
			(parent (find-parent node preds))
		)

		(if (null parent)
			nil
			(cons parent (backtrack-path parent preds))
		)
	)
)

; IMPLICTLY TESTED via pq-add-one & pq-add-many
; tests wheter entry should be added at the head of pq
(defun pq-is-lesseql (pq entry)
	(<= (cadr entry) (cadr (car pq)))
)

; TESTED - works
; entry is '(item priority)
(defun pq-add-one (pq entry)
	(cond
		((null pq) (list entry))
		(t
			(if (pq-is-lesseql pq entry)
				; then
				(cons entry pq)
				;else
				(cons (car pq) (pq-add-one (cdr pq) entry))
			)
		)
	)
)

; TESTED - works
; add all entries to pq and return pq
(defun pq-add-many (pq entries)
	(cond
		((null entries) pq)
		(t
			(pq-add-many (pq-add-one pq (car entries)) (cdr entries))
		)
	)

	;(foldr (lambda (sc pqa)
	;			(pq-add-one pqa sc)
	;	   ) pq entires)
)
