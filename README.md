			 .oooooo..o                     oooo   o8o                      
			d8P'    `Y8                     `888   `"'                      
			Y88bo.       .ooooo.   .oooo.    888  oooo   .oooo.o oo.ooooo.  
			 `"Y8888o.  d88' `"Y8 `P  )88b   888  `888  d88(  "8  888' `88b 
			     `"Y88b 888        .oP"888   888   888  `"Y88b.   888   888 
			oo     .d8P 888   .o8 d8(  888   888   888  o.  )88b  888   888 
			8""88888P'  `Y8bod8P' `Y888""8o o888o o888o 8""888P'  888bod8P' 
			                                                      888       
			                                                     o888o      

A lisp interpreter and compiler written in Scala, inspired by [Peter Norvig](http://norvig.com/lispy.html)

Download
========

You can either clone this repo and use `sbt` or you can download the precompiled
`jar` and execute that.

To be able to compile the resulting scala code, you should also download 
`compiled_builtins.scala` and place it in the same directory as the compiled 
`.scala` file.

Interpeter
==========

It's a bit incomplete and probably buggy, but it works good enough to for most applications:

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

Compiler
========

Instead of taking the traditional route and compiling to byte code (which 
Clojure already does), I decided to compile to Scala instead. Just use `scalisp` 
like you would for interpetation, but add the `-c` switch.

For example, the compiled merge sort looks like this:

```scala
def merge(a: Any, b: Any): Any = {
  if(length(a) < 1l) {
    b
  } else {
    if(length(b) < 1l) {
      a
    } else {
      if(car(a) < car(b)) {
        car(a) :: merge(cdr(a), b)
      } else {
        car(b) :: merge(a, cdr(b))
      }
    }
  }
}

def msort(list: Any): Any = {
  if(length(list) <= 1l) {
    list
  } else {
    {
      var split = (length(list) / 2l)
      merge(msort(subseq(list, 0l, split)), msort(subseq(list, split)))
    }
  }
}
```

If you specify a whole file, you'll get a complete `.scala` file back, ready to
be compiled. If you just execute `scalisp -c`, you get a REPL, which simply 
compiles snippets of code.