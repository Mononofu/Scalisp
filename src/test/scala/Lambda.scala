package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LambdaSpec extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"Lambdas" should "be allowed" in {
		parser.executeLine("(define test (lambda (r) (* 3.141592653 (* r r))))")
	}

	it should "execute correctly" in {
		parser.executeLine("(define square (lambda (x) (* x x)))")
		parser.executeLine("(square 4)") should equal (16)
	}


}
