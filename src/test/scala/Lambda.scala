package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LambdaSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"Lambdas" should "be allowed" in {
		repl.executeLine("(define test (lambda (r) (* 3.141592653 (* r r))))")
	}

	it should "execute correctly" in {
		repl.executeLine("(define square (lambda (x) (* x x)))")
		repl.executeLine("(square 4)") should equal (16)
	}


}
