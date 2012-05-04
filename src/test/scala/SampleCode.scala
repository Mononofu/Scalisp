

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

}
