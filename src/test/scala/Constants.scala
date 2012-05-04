package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConstantSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"A number" should "evaluate to itself" in {
		repl.executeLine("5") should equal (5)
		repl.executeLine("-7") should equal (-7)
		repl.executeLine("75.34") should equal (75.34)
		repl.executeLine("8e23") should equal (8e23)
		repl.executeLine("-8e-23") should equal (-8e-23)
		repl.executeLine("-75.34") should equal (-75.34)
	}

	"A string" should "also evaluate to itself" in {
		repl.executeLine("\"hello\"") should equal ("hello")
		repl.executeLine("\"\"") should equal ("")
	}

	"Boolean values" should "work" in {
		repl.executeLine("true") should equal (true)
		repl.executeLine("false") should equal (false)
	}


}
