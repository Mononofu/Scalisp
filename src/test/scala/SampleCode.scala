

package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SampleCode extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"Factorial" should "work" in {
		repl.executeLine("(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))")

		repl.executeLine("(fact 6)") should equal (720)
		repl.executeLine("(fact 10)") should equal (3628800)
		repl.executeLine("(fact 0)") should equal (1)
	}

  "Fibonacci numbers" should "also work" in {
    repl.executeLine("(define fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))))")

    repl.executeLine("(fib 1)") should equal (1)
    repl.executeLine("(fib 2)") should equal (1)
    repl.executeLine("(fib 10)") should equal (55)
    repl.executeLine("(fib 15)") should equal (610)
  }

  "Merge sort" should "work" in {

    repl.execute(
      """
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
      )""")

    repl.executeLine("(merge '(1 4) '())") should equal (List(1, 4))
    repl.executeLine("(merge '() '(1 4))") should equal (List(1, 4))
    repl.executeLine("(merge '(1 4) '(2 3))") should equal (List(1, 2, 3, 4))

    repl.executeLine("(msort '(5 7 2 1 3 4 6))") should equal (List(1, 2, 3, 4, 5, 6, 7))
  }

}
