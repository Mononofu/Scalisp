(defun __length (list acc)
  (if (= list '())
    acc
    (__length (cdr list) (+ 1 acc))
  )
)

(defun length (list)
  (__length list 0)
)



(defun __reverse (l acc)
  (if (= '() l)
    acc
    (__reverse (cdr l) (cons (car l) acc))
  )
)

(defun reverse (l)
  (__reverse l '())
)



(defun __subseq (list start stop acc)
  (if (> start 0)
    (subseq (subseq list start) 0 (- stop start) acc)
    (if (<= stop 0)
      (reverse acc)
      (__subseq (cdr list) 0 (- stop 1) (cons (car list) acc))
    )
  )
)

(defun subseq (list start)
  (if (<= start 0)
    list
    (subseq (cdr list) (- start 1))
  )
)

(defun subseq (list start stop)
  (__subseq list start stop '())
)



(defun __range (start stop acc)
  (if (<= stop start)
    (reverse acc)
    (__range (+ start 1) stop (cons start acc))
  )
)

(defun range (stop)
  (__range 0 stop '())
)

(defun range (start stop)
  (__range start stop '())
)


(defun map (f seq)
  (foldr (lambda (elem acc) (cons (f elem) acc) ) '() seq)
)



(defun foldl (f acc seq)
  (if (= '() seq)
    acc
    (foldl f (f acc (car seq)) (cdr seq))
  )
)

(defun foldr (f acc seq)
  (if (= '() seq)
    acc
    (foldr f (f (last seq) acc) (init seq))
  )
)

(defun reduce (f seq)
  (foldl f (car seq) (cdr seq))
)

(defun filter (f seq)
  (foldr (lambda (elem acc) (if (f elem) (cons elem acc) acc)) '() seq)
)

