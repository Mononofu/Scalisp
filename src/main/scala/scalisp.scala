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
  def define(k: String, v: Any) { 
    v match {
      case f: Function =>
        map.get(k) match {
          case Some(t: FunctionTable) => 
            t.add(f)
          case _ => 
            val t = new FunctionTable()
            t.add(f)
            map(k) = t
        }
      case _ => map(k) = v 
    }
  }
  def apply(k: String): Option[Any] = map.get(k) match {
    case None => parent match {
      case null => None
      case _ => parent(k)
    }
    case v => v
  }

  def getFunction(k: String, arity: Int): Option[Function] = map.get(k) match {
    case None => parent match {
      case null => None
      case _ => parent.getFunction(k, arity)
    }
    case Some(t: FunctionTable) => 
      t(arity) match {
      case None => parent match {
        case null => None
        case _ => parent.getFunction(k, arity)
      }
      case f => f
    }
    case v => throw new TypeError(k + " is not a function, " + v)
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

  // load builtins defined in lisp
  execute(io.Source.fromFile("builtins.l").mkString)

  def execute(l: String) = {
    val ast = parser.parse(l.replaceAll(";[^\n$]*", " ").replace("\n", " "))
    ast.map(e => Interpreter.eval(e, defaultEnv))
  }

  def executeLine(l: String) = {
    val r = execute(l)
    if(r.length < 1) () else r.last
  }
}

object Scalisp {
  val repl = new REPL()

  def main(args: Array[String]) {
    if(args.length > 0) {
      val input = io.Source.fromFile(args(0)).mkString
      try {
        repl.execute(input)
      }
      catch {
        case e: InterpreterException => println(e)
        case e: MatchError => println(e)
      }
    }
    else {
      val consoleReader = new jline.console.ConsoleReader()
      
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
  }
}