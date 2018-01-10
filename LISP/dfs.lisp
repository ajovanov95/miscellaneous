(defun opposite (x)
  (if (eq x 'w) 'e 'w)
)

(defun make-state (f w g c)
  (list f w g c)
)

(defun get-farmer (state)
  (nth 0 state)
)

(defun get-wolf (state)
  (nth 1 state)
)

(defun get-goat (state)
  (nth 2 state)
)

(defun get-cabbage (state)
  (nth 3 state)
)

; wolf - goat
(defun unsafe1 (state)
  (and
    (eq (get-wolf state) (get-goat state))
    (not (eq (get-farmer state) (get-wolf state)) )
  )
)

; goat - cabbage
(defun unsafe2 (state)
  (and
    (eq (get-goat state) (get-cabbage state))
    (not (eq (get-farmer state) (get-goat state)) )
  )
)

; farmer sam
(defun unsafe3 (state)
  (equal state '(w e e e))
)

(defun safe (state)
  (cond
    ((unsafe1 state) nil)
    ((unsafe2 state) nil)
    ((unsafe3 state) nil)
    (t state)
  )
)

; akcii
(defun farmer-transfer (state)
  (safe (make-state
      (opposite (get-farmer state))
      (get-wolf state)
      (get-goat state)
      (get-cabbage state)
  ))
)

(defun wolf-transfer (state)
  (if (equal (get-farmer state) (get-wolf state))
    (safe (make-state
      (opposite (get-farmer state))
      (opposite (get-wolf state))
      (get-goat state)
      (get-cabbage state)
    ))

    nil
  )
)

(defun goat-transfer (state)
  (if (equal (get-farmer state) (get-goat state))

    (safe (make-state
        (opposite (get-farmer state))
        (get-wolf state)
        (opposite (get-goat state))
        (get-cabbage state)
    ))

    nil
  )
)

(defun cabbage-transfer (state)
  (if (equal (get-farmer state) (get-wolf state))

    (safe (make-state
        (opposite (get-farmer state))
        (get-wolf state)
        (get-goat state)
        (opposite (get-cabbage state))
    ))

    nil
  )
)

(defun dfs (state goal been-list)
  (cond
    ((null state) nil)

    ((equal state goal) (reverse (cons goal been-list )) )

    ((not (member state been-list :test #'equal))
      (or
        (dfs (farmer-transfer state) goal (cons state been-list))
        (dfs (wolf-transfer state) goal (cons state been-list))
        (dfs (goat-transfer state) goal (cons state been-list))
        (dfs (cabbage-transfer state) goal (cons state been-list))
      )
    )
  )
)

(defun solve ()
  (dfs '(e e e e) '(w w w w) '())
)
