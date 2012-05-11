package Scalisp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Compiler extends FlatSpec with ShouldMatchers {
  val repl = new REPL()

  "Function arities" should "be recognized correctly" in {
    val ast = List[Any]("begin", List[Any]("print", 1, 4), List[Any]("echo", 3, 3.0))
    ScalispCompiler.isFunction("print", ast) should equal (2)
    ScalispCompiler.isFunction("+", ast) should equal (-1)

    val ast2 = List[Any]("if", 
      List[Any]("=", List(), "seq"),
      List(),
      List[Any]("cons",
        List[Any]("f",
          List[Any]("car", "seq")),
        List[Any]("map",
          "f",
          List[Any]("cdr", "seq"))))


    ScalispCompiler.isFunction("f", ast2) should equal (1)

    val ast3 = List("foldl",
      "f",
      List("car", "seq"),
      List("cdr", "seq"))

    ScalispCompiler.usedInFunction("f", ast3).get should equal (ScalispCompiler.UsedIn("foldl", 0))
    ScalispCompiler.usedInFunction("car", ast3) should equal (None)
  }

  "Compiled builtins" should "still work correctly" in {
    val l = CompiledApp.CompiledBuiltins.range(5l)
    l should equal (List(0, 1, 2, 3, 4))
    CompiledApp.CompiledBuiltins.length(l) should equal (5l)
  }


}
