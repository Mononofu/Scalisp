package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ProcedureSpec extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"Addition and substraction" should "work as expected" in {
		parser.executeLine("(+ 4 3 5 3)") should equal (15)
		parser.executeLine("(+ 10 -3 8)") should equal (15)

		parser.executeLine("(- 10 -3 8)") should equal (5)
	}

	it should "still work if only one element is passed" in {
		parser.executeLine("(+ 7)") should equal (7)

		parser.executeLine("(- 7)") should equal (7)
	}

	"Comparisons" should "work normally" in {
		parser.executeLine("(< 2 3)") should equal (true)
		parser.executeLine("(< 4 3)") should equal (false)
		parser.executeLine("(< 3 3)") should equal (false)
		parser.executeLine("(< 1 2 3 4 5)") should equal (true)
		parser.executeLine("(< 3 2 1 4)") should equal (false)

		parser.executeLine("(> 2 3)") should equal (false)
		parser.executeLine("(> 4 3)") should equal (true)
		parser.executeLine("(> 3 3)") should equal (false)
		parser.executeLine("(> 5 4 3 2 1)") should equal (true)
		parser.executeLine("(> 4 2 3 1)") should equal (false)

		parser.executeLine("(= 2 3)") should equal (false)
		parser.executeLine("(= 3 3)") should equal (true)
		parser.executeLine("(= 1 2 3)") should equal (false)
		parser.executeLine("(= 3 3 3 3)") should equal (true)
	}

}
