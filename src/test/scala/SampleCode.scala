

package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SampleCode extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"Factorial" should "work" in {
		parser.executeLine("(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))")

		parser.executeLine("(fact 6)") should equal (720)
		parser.executeLine("(fact 10)") should equal (3628800)
		parser.executeLine("(fact 0)") should equal (1)
	}


}
