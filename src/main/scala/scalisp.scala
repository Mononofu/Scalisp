package Scalisp

import scala.tools.jline
import org.clapper.argot._
import java.io.File
import java.io.PrintWriter

class Env(parent: Env) {
  val map: collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()

  override def toString = map.mkString("\n")

  def update(k: String, v: Any) { 
    map.get(k) match {
      case Some(_) => map(k) = v
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
      "true" -> true, "false" -> false, "unit" -> (),
      "+" -> "+", "-" -> "-", "*" -> "*", "/" -> "/", "%" -> "%",
      ">" -> ">", ">=" -> ">=", "<" -> "<", "<=" -> "<=",  "=" -> "=",  "!=" -> "!=",
      "min" -> "min", "max" -> "max"
    ) 
  }

  // load builtins defined in lisp
  execute(io.Source.fromFile("builtins.lisp").mkString)

  def execute(l: String) = {
    val ast = LispParser.parse(l.replaceAll(";[^\n$]*", " ").replace("\n", " "))
    Preprocessor.process(ast).map(e => Interpreter.eval(e, defaultEnv))
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

  val builtinNames = List("car", "cdr", "cons", "append", "list", "shuffle",
    "print", "to-string", "concat", "atom")

  def main(args: Array[String]) {
    try {
      parser.parse(args)
      input.value match {
        case List() => 
          val consoleReader = new jline.console.ConsoleReader()
          val completer = new StringsCompleter(builtinNames ++ repl.defaultEnv.map.keys)
          consoleReader.addCompleter(completer);
          Iterator.continually(consoleReader.readLine("scalisp> ")).takeWhile(_ != "").foreach {
            case "exit" | null => sys.exit(0)
            case line => 
              try {
                var src = line
                // make sure expressions are balanced
                while(src.count(_ == '(') != src.count(_ == ')')) {
                  val in = consoleReader.readLine("       | " )

                  if(in == null || in.length == 0)
                    // this will throw an exception
                    println(repl.executeLine(src))
                  src += " " + in
                }
                compile.value match {
                  case Some(true) => println(ScalispCompiler.compileLine(src))
                  case _ => 
                    println("=> " + repl.executeLine(src))
                    completer.setStrings(repl.defaultEnv.map.keys)
                }
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
              case Some(true) => 
                val code = ScalispCompiler.compile(src, filename)
                val out = new PrintWriter( new File(filename.split('.').init :+ "scala" mkString ".") )
                try { 
                  out.print( code ) 
                } finally{ 
                  out.close 
                }
              case _ => repl.execute(src)
            }              
        }
      }
    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }
}