package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConstantSpec extends FlatSpec with ShouldMatchers {
	val parser = new LispParser()

	"A number" should "evaluate to itself" in {
		parser.executeLine("5") should equal (5)
		parser.executeLine("-7") should equal (-7)
		parser.executeLine("75.34") should equal (75.34)
		parser.executeLine("8e23") should equal (8e23)
	}

	"A string" should "also evaluate to itself" in {
		parser.executeLine("\"hello\"") should equal ("hello")
		parser.executeLine("\"\"") should equal ("")
	}

	"Boolean values" should "work" in {
		parser.executeLine("true") should equal (true)
		parser.executeLine("false") should equal (false)
	}


}
