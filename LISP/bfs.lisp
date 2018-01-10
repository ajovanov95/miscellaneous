(load "search-utils.lisp")

; Breadth-first search nad proizvolno drvo na sostojbi
; Vraka optimalen pat od korenot do nekoja celna sostojba
; ako e cenata na grankite e 1 za site
(defun bfs (start generator goaltest)
	(let*
		(
			(node-preds (bfsi (list start) start generator goaltest '()))
			(goalnode (car node-preds))
			(preds (cadr node-preds))
		)
		; action
		(reverse (cons goalnode (backtrack-path goalnode preds)))
	)
)

; Ne drzi closed list, pretpostavuva deka nema da ima ciklusi (mozebi losa ideja)
; bfs - implementation
(defun bfsi (to-expand parent generator goaltest preds)
	(setq node (car to-expand))
	(cond
		; if reached goal node return node & all preds info
		((eql (funcall goaltest node) t)
			(list node preds)
		)

		; if no goal found look at all the succesors in order
		(t
			(let*
				(
					(new-states    (funcall generator node))
					(new-to-expand (append (cdr to-expand) new-states))
					(new-preds
						(append
							(mapcar (lambda (n) (list n node)) new-states)
							preds
						)
					)
				)

				(bfsi
					new-to-expand
					node
					generator
					goaltest
					new-preds
				)
			)
		)
	)
)
