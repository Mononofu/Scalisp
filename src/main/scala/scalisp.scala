package Scalisp

import scala.tools.jline
import org.clapper.argot._
import java.io.File

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
    case _ => None
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
  execute(io.Source.fromFile("builtins.lisp").mkString)

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
  import ArgotConverters._
  val parser = new ArgotParser("scalisp", preUsage=Some("Version 1.0"))
  val compile = parser.flag[Boolean](List("c", "compile"),
                                   "Compile instead of interpret")
  val input = parser.multiParameter[String]("input",
                                        "Input files to read. If not " +
                                        "specified, use stdin.",
                                        true) { 
    case (s, opt) =>
      val file = new File(s)
      if (! file.exists)
          parser.usage("Input file \"" + s + "\" does not exist.")
      s
  }

  val repl = new REPL()

  def main(args: Array[String]) {
    try {
      parser.parse(args)
      input.value match {
        case List() => 
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
        case l: List[String] => l.foreach { 
          case filename => 
            val src = io.Source.fromFile(filename).mkString
            compile.value match {
              case Some(true) => Compiler.compile(src)
              case _ => repl.execute(src)
            }              
        }
      }
    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }
}