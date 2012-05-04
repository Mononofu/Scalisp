package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class VariableSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"A variable" should "save its content" in {
		repl.executeLine("(define a 5)")
		repl.executeLine("a") should equal (5)

		repl.executeLine("(define b (+ a a))")
		repl.executeLine("b") should equal (10)
	}


}
