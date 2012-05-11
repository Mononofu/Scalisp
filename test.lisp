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