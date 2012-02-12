package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IfSpec extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"An If" should "select the first branch if the condition is true" in {
		parser.executeLine("(if true 1 0)") should equal (1)
		parser.executeLine("(if (< 1 2) 1 0)") should equal (1)
	}

	it should "select the second branch if the condition is false" in {
		parser.executeLine("(if false 1 0)") should equal (0)
		parser.executeLine("(if (= 3 2) 1 0)") should equal (0)
	}

}
