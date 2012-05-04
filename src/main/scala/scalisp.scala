package Scalisp

import scala.tools.jline

class Env(parent: Env) {
  val map = collection.mutable.Map[String, Any]()

  def update(k: String, v: Any) { 
    map.get(k) match {
      case Some(_) => println("updating to: " + v); map(k) = v
      case None => parent match {
        case null => throw new VariableNotFound(k)
        case p => p(k) = v
      }
    } 
  }
  def define(k: String, v: Any) { map(k) = v }
  def apply(k: String): Option[Any] = map.get(k) match {
    case None => parent match {
      case null => None
      case _ => parent(k)
    }
    case v => v
  }
}

class REPL {
  val defaultEnv = new Env(null) {
    override val map = collection.mutable.Map[String, Any]( 
      "true" -> true,
      "false" -> false
    ) 
  }

  val parser = new LispParser()

  def execute(l: String) = l.split("\n").map(_.trim).filter(_.length > 0).map {
    case line => executeLine(line)
  }

  def executeLine(l: String) = {
    val ast = parser.parse(l)
    val v = Interpreter.eval(ast, defaultEnv)
    v
  }
}

object Scalisp extends App {
  val consoleReader = new jline.console.ConsoleReader()
  val repl = new REPL()

  Iterator.continually(consoleReader.readLine("scalisp> ")).takeWhile(_ != "").foreach {
    case "exit" | null => sys.exit(0)
    case line => 
      try {
        println(repl.executeLine(line))
      }
      catch {
        case e: InterpreterException => println(e)
        case e: MatchError => println(e)
      }
  }

}