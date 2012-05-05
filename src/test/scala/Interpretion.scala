package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Interpretation extends FlatSpec with ShouldMatchers {
  val repl = new REPL()

  "Functions" should "be quotable" in {
    repl.executeLine("'+") should equal ("+")
    repl.executeLine("(foldl '+ 0 '(1 2 3 4 5))") should equal (15)
  }

  "Multiple Functions with different arities" should "not shadow each other" in {
    repl.execute("""
      (defun test (a) 1)
      (defun test (a b) 2)
      (defun test (a b c) 3)
      (define test (lambda (a b c d) 4))
      """)

    evaluating { repl.executeLine("(test)") } should produce [MethodNotFound]
    repl.executeLine("(test 0)") should equal (1)
    repl.executeLine("(test 0 0)") should equal (2)
    repl.executeLine("(test 0 0 0)") should equal (3)
    repl.executeLine("(test 0 0 0 0)") should equal (4)
  }

}
