package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ProcedureSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"Multi-line code" should "not crash" in {
		repl.execute("""
			(define a 1)
			(define b 2)
			(define l '(1 2 3 4))
			""")
	}

	"Addition and substraction" should "work as expected" in {
		repl.executeLine("(+ 4 3 5 3)") should equal (15)
		repl.executeLine("(+ 10 -3 8)") should equal (15)

		repl.executeLine("(- 10 -3 8)") should equal (5)

		repl.executeLine("(+ a b)") should equal (3)
		repl.executeLine("(- a b)") should equal (-1)
	}

	it should "still work if only one element is passed" in {
		repl.executeLine("(+ 7)") should equal (7)

		repl.executeLine("(- 7)") should equal (7)
	}

	"Comparisons" should "work normally" in {
		repl.executeLine("(< 2 3)") should equal (true)
		repl.executeLine("(< 4 3)") should equal (false)
		repl.executeLine("(< 3 3)") should equal (false)
		repl.executeLine("(< 1 2 3 4 5)") should equal (true)
		repl.executeLine("(< 3 2 1 4)") should equal (false)

		repl.executeLine("(< a b)") should equal (true)
		repl.executeLine("(< b a)") should equal (false)


		repl.executeLine("(> 2 3)") should equal (false)
		repl.executeLine("(> 4 3)") should equal (true)
		repl.executeLine("(> 3 3)") should equal (false)
		repl.executeLine("(> 5 4 3 2 1)") should equal (true)
		repl.executeLine("(> 4 2 3 1)") should equal (false)

		repl.executeLine("(> a b)") should equal (false)
		repl.executeLine("(> b a)") should equal (true)


		repl.executeLine("(= 2 3)") should equal (false)
		repl.executeLine("(= 3 3)") should equal (true)
		repl.executeLine("(= 1 2 3)") should equal (false)
		repl.executeLine("(= 3 3 3 3)") should equal (true)

		repl.executeLine("(= a b)") should equal (false)
		repl.executeLine("(= a a)") should equal (true)
	}

	"car" should "return the head of a list" in {
		repl.executeLine("(car '(1 2 3 4))") should equal (1)
		repl.executeLine("(car l)") should equal (1)
	}

	it should "throw a TypeError on non-lists" in {
		evaluating { repl.executeLine("(car 1)") } should produce [TypeError]
	}

	"cdr" should "return the tail of a list" in {
		repl.executeLine("(cdr '(1 2 3 4))") should equal (List(2, 3, 4))
		repl.executeLine("(cdr l)") should equal (List(2, 3, 4))
	}

	it should "throw a TypeError on non-lists" in {
		evaluating { repl.executeLine("(cdr 1)") } should produce [TypeError]
	}

	"append" should "merge two or more lists" in {
		repl.executeLine("(append '(1 2) '(3 4))") should equal (List(1, 2, 3, 4))
		repl.executeLine("(append '(1 2) '(3 4) '(5 6) '(7 8))") should equal (List(1, 2, 3, 4, 5, 6, 7, 8))
		repl.executeLine("(append l l)") should equal (List(1, 2, 3, 4, 1, 2, 3, 4))
	}

	it should "throw a TypeError on non-lists" in {
		evaluating { repl.executeLine("(append 1 2 3)") } should produce [TypeError]
		evaluating { repl.executeLine("(append '(1 2) 2 3)") } should produce [TypeError]
	}

}
