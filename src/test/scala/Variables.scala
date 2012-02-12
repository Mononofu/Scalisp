package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class VariableSpec extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"A variable" should "save its content" in {
		parser.executeLine("(define a 5)")
		parser.executeLine("a") should equal (5)

		parser.executeLine("(define b (+ a a))")
		parser.executeLine("b") should equal (10)
	}


}
