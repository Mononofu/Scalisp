(defun length (list)
  (if (= list '())
    0
    (+ 1 (length (cdr list)))
  )
)

(defun reverse (l)
  (if (= '() l)
    l
    (append (reverse (cdr l)) (list (car l)))
  )
)

(defun subseq (list start)
  (if (<= start 0)
    list
    (subseq (cdr list) (- start 1))
  )
)

(defun subseq (list start stop)
  (if (> start 0)
    (subseq (subseq list start) 0 (- stop start))
    (if (<= stop 0)
      '()
      (cons (car list) (subseq (cdr list) 0 (- stop 1)))
    )
  )
)


(defun range (stop)
  (range 0 stop)
)

(defun range (start stop)
  (if (<= stop start)
    '()
    (cons start (range (+ start 1) stop))
  )
)

