package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BugFiexes extends FlatSpec with ShouldMatchers {
  val repl = new REPL()

  // https://github.com/Mononofu/Scalisp/issues/1
  "Variables set in child scopes" should "change the parent scope too" in {
    repl.execute("""
      (define x 3)
      (defun f (n) (set! x n))
      """)
    repl.executeLine("x") should equal (3)
    repl.executeLine(("(f 2)"))
    repl.executeLine("x") should equal (2)
  }


}
