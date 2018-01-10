(load "search-utils.lisp")

; Uniform Cost Search
; Vraka optimalen pat za bilo koe proizvolno drvo na sostojbi so ceni na pat != 1
(defun ucs (start generator goaltest)
	(let*
		(
			(node-cost-preds (ucsi (list (list start 0)) generator goaltest start '()))
			(goalnode (car node-cost-preds))
			(full-cost (cadr node-cost-preds))
			(preds (caddr node-cost-preds))
			(path (reverse (cons goalnode (backtrack-path goalnode preds))))
		)
		(list full-cost path)
	)
)

; pq : ('(state g-cost) '(state2 g-cost2) ... '(staten g-costn))
; s.t. g-cost <= g-cost2 <= ... <= g-costn
; pq = priority queue
; generator returns a list of the following format:
; ( (state1 cost1) (state2 cost2) ... (statem costm))
; where costi is the cost from *parent node* (not root) to child i
; this functions generates the g-cost automatically
; preds, parent & goaltest are the same from bfs
(defun ucsi (pq generator goaltest parent preds)
	; current node & its cost from root
	(setq node (car (car pq))
	(setq cost-from-root (cadr (car pq)))
	(setq pq (cdr pq)) ; remove the head of pq

	(cond
		; if reached goal node return node, full-cost & preds
		((eql (funcall goaltest node) t)
			(list node cost-from-root preds)
		)

		; if no goal found look at all the succesors in order of lowest cost from root
		(t
			(let*
				(

					(new-states-cost1 (funcall generator node))
					; dodadi ja cenata od korenot na cenata sto ja dobivme od generatorot
					(new-states-cost2
						(mapcar
							(lambda (sc)
								(list
									(
										(car sc) ; sostojbata n
										(+ cost-from-root (cadr sc)) ; cenata od korenot do n
										; za A* tuka se dodava i hevristickata f-ja
									)
								) ;end of list
							) ; end of lambda
						) new-states-cost1
					)

					(new-pq (pq-add-many pq new-states-cost2))

					(new-preds
						(append
							(mapcar (lambda (sc) (list (car sc) node)) new-states-cost1)  ; sc is '(state cost)
							preds
						)
					)
				)

				(ucsi
					new-pq
					generator
					goaltest
					node
					new-preds
				)
			)
		)
	)
)
