package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Parsing extends FlatSpec with ShouldMatchers {
  val repl = new REPL()

  "Statements" should "be able to span multiple lines" in {
    repl.executeLine("""
      (+
        1
        2)
      """) should equal (3)
  }

  "Comments" should "be ignored" in {
    repl.executeLine("(+ 1 2) ; a comment") should equal (3)
    repl.execute("""
      ; a multiline comment
      ; it goes on
      ; and on
      (* 2 3) ; more Comments
      ; this shouldn't execute: (+ 10 20)
      """) should equal (List(6))
  }

}
