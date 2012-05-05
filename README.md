			 .oooooo..o                     oooo   o8o                      
			d8P'    `Y8                     `888   `"'                      
			Y88bo.       .ooooo.   .oooo.    888  oooo   .oooo.o oo.ooooo.  
			 `"Y8888o.  d88' `"Y8 `P  )88b   888  `888  d88(  "8  888' `88b 
			     `"Y88b 888        .oP"888   888   888  `"Y88b.   888   888 
			oo     .d8P 888   .o8 d8(  888   888   888  o.  )88b  888   888 
			8""88888P'  `Y8bod8P' `Y888""8o o888o o888o 8""888P'  888bod8P' 
			                                                      888       
			                                                     o888o      

A lisp interpreter written in Scala, inspired by [Peter Norvig](http://norvig.com/lispy.html)

It's very incomplete and probably buggy, but it works good enough to calculate
factorials and stuff: 

```lisp
(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))
(fact 10) 
```

produces `3628800`

Also, Merge sort can be implemented without a problem:

```lisp
; note that you can also use comments
; and split functions over multiple lines for readability
(define msort 
  (lambda (list) 
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
)

; ordering is not important, as functions are evaluated lazily
(define merge
  (lambda (a b)
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
)
```

Usage is just like you'd expect

```lisp
(msort '(5 7 2 1 3 4 6))
```

And results in `'(1 2 3 4 5 6 7)`

It's also possible to execute files, simply do

	scalisp filename.l

or, from the sbt-console

	run filename.l