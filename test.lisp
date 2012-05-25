; this file is a collection of various code to test the intepreter 
; and show how to code for it

(print "hello world")

(defun fib (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(print "20th fibonacci number: " (fib 10))


; note that you can also use comments
; and split functions over multiple lines for readability
(defun msort (list) 
  (if (<= (length list) 1) 
    list 
    (begin 
      (define split (/ (length list) 2)) 
      (merge 
        (msort (subseq list 0 split)) 
        (msort (subseq list split)) 
      ) 
    ) 
  ) 
)

; ordering is not important, as functions are evaluated lazily
(defun merge (a b)
  (if (< (length a) 1)
    b
    (if (< (length b) 1)
      a
      (if (< (car a) (car b))
        (cons (car a) (merge (cdr a) b))
        (cons (car b) (merge a (cdr b)))
      )
    )
  )
)

(define l (shuffle (range 20)))
(print "let's take a random list: " l)
(print "and sort it: " (msort l))

(print (length (range 10)))

(print "test higher-order functions")
(print (map (lambda (a) (* a a)) (range 10)))
(print (reduce * (range 1 10)))

(print "complex arithmetic")
(print (+ (* (+ 1 2) (- 4 1)) 10))

(print "test long/double typing")
(defun echo (n) (+ n 1))
(print (echo 10))
(print (echo 20.5))

(print "macros")
(defmacro square (X) (* ,X ,X))
(print (square 5))

(defmacro square+ (X) 
  (let (x) (,X) (* x x)))
(print (square+ (begin (print "executed") 5)))


(defun unless_bad (cond exp)
  (if cond unit exp))
(unless_bad (= 5 5) (print "executed branch"))

(defmacro unless (COND EXP)
  (if ,COND unit ,EXP))
(unless (= 5 5) (print "executed branch"))

(defun map_good (f l acc)
  (if (= l '())
    acc
    (map_good f (cdr l) (cons (f (car l)) acc))
  )
)

;(map_good (lambda (x) (+ x 1)) (range 10000000) '())

; naive sum, will cause stack overflow for n = 10.000
(defun sum_to (n)
  (if (= n 0)
    0
    (+ n (sum_to (- n 1)))))

; tail-call version, won't overflow
(defun sum_to_good (n acc)
  (if (= n 0)
    acc
    (sum_to_good (- n 1) (+ n acc))))

(print "tail-recursive sum: " (sum_to_good 10000 0))

; another tail-cail test
;(subseq (range 10200) 10000 10009)