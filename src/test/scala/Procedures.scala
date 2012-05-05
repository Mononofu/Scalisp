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

	"range" should "create a range of numbers" in {
		repl.executeLine("(range 5)") should equal (List(0, 1, 2, 3, 4))
		repl.executeLine("(range 5 10)") should equal (List(5, 6, 7, 8, 9))
	}

	"length" should "return the length of a list" in {
		repl.executeLine("(length (range 10))") should equal (10)
	}

	"map" should "map the values of a list" in {
		repl.executeLine("(map (lambda (a) (+ a 5)) (range 5))") should equal (List(5, 6, 7, 8, 9))
	}

	"foldl" should "fold a list" in {
		repl.executeLine("(foldl '+ 0 (range 5))") should equal (10)
		repl.executeLine("(foldl '* 1 (range 1 5))") should equal (24)
	}

	"reduce" should "should act like fold, but with no initial value" in {
		repl.executeLine("(reduce '+ (range 5))") should equal (10)
		repl.executeLine("(reduce '* (range 1 5))") should equal (24)
	}

	"filter" should "filter a list" in {
		repl.executeLine("(filter (lambda (a) (> a 5)) (range 10))") should equal (
			List(6, 7, 8, 9))
	}



}
