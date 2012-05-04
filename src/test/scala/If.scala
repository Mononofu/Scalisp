package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IfSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"An If" should "select the first branch if the condition is true" in {
		repl.executeLine("(if true 1 0)") should equal (1)
		repl.executeLine("(if (< 1 2) 1 0)") should equal (1)
	}

	it should "select the second branch if the condition is false" in {
		repl.executeLine("(if false 1 0)") should equal (0)
		repl.executeLine("(if (= 3 2) 1 0)") should equal (0)
	}

}
