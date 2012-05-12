package Scalisp

class CompilerError(s: String) extends Exception(s) { }

object ScalispCompiler {
  var global_functions = List[String]()
  var higher_order_funs = collection.mutable.Map[String, 
    collection.mutable.Map[Int, Int]]().withDefault(_ =>  collection.mutable.Map[Int, Int]())


  def compile(src: String, filename: String) = {
    // strip comments and replace newlines with spaces
    val ast = LispParser.parse(src.replaceAll(";[^\n$]*", " ").replace("\n", " "))

    val code = Preprocessor.process(ast).map(e => process(e)).filter(_.length > 0).mkString("\n\n  ")

    """
object %s extends App {
  import CompiledApp.CompiledBuiltins._
  import CompiledApp.Helper._

  // user methods
  %s

  // main code
  %s
}
  """.format(filename, global_functions.mkString("\n\n  "), code)
  }

  def compileLine(src: String) = {
    global_functions = List()
    val ast = LispParser.parse(src.replaceAll(";[^\n$]*", " ").replace("\n", " "))
    val code = Preprocessor.process(ast).map(e => process(e)).filter(_.length > 0).mkString("\n\n  ")
    global_functions.mkString("\n\n  ") + code
  }

  def isFunction(name: String, body: Any): Int = body match {
    case n :: tail if n == name => tail.length
    case Nil => -1
    case l: List[Any] => l.map(e => isFunction(name, e)).max
    case _ => -1
  }

  case class UsedIn(name: String, argPos: Int)
  def usedInFunction(name: String, exp: Any): Option[UsedIn] = exp match {
    case Nil => None
    case l: List[Any] => 
      if(l.tail.contains(name))
        l.head match {
          case s: String => Some( UsedIn(s, l.indexOf(name) - 1) )
          case _ => None
        }
        
      else
        l.tail.map(e => usedInFunction(name, e)).flatten.headOption
    case _ => None
  }


  def process(exp: Any, indent: String = "  "): String = exp match {
    case l: List[Any] => l.head match {
      // special forms
      case "if" => ("if(%s) {\n" + indent + "  %s\n" + indent +"} else {\n" + indent + "  %s\n" + indent + "}").format( process(l(1)), process(l(2), indent + "  "), process(l(3), indent + "  ") )
      case "define" => l(1) match {
        case name: String => "var %s = %s".format(name, process(l(2)))
        case _ => throw new CompilerError("variable name has to be a string")
      }
      case "set!" => l(1) match {
        case name: String => "%s = %s".format(name, process(l(2)))
        case _ => throw new CompilerError("variable name has to be a string")
      }
      case "begin" => l.tail.map(e => process(e, indent + "  ")).mkString("{\n  " + indent, "\n  " + indent, "\n" + indent + "}")
      case "quote" => stringify(l(1))
      case "lambda" => l(1) match {
        case parms: List[Any] => 
          val p = parms.map {
            case n: String => n
            case _ => throw new CompilerError("parm names have to be strings")
          }
          val args = p.map(_ + ": Any").mkString(", ")
          val body = process(l(2), indent + "  ")
          if(body.length < 20)
            "(%s) => { %s }".format(args, body)
          else
            ("(%s) => {\n" + indent + "  %s\n" + indent + "}").format(args, body)
        case _ => throw new CompilerError("lambda arguments have to be a list")
      }
      case "let" => l(1) match {
      case names: List[String] => l(2) match {
        case vals: List[Any] => 
          val body = l(3)

          val prelude = names.zip(vals).map {
            case (param: String, value: Any) => "val %s = %s".format(param, process(value, indent + "  "))
          }
          ("{\n" + indent + "  %s\n" + indent + "  %s\n" + indent + "}").format(prelude.mkString("\n  " + indent), process(body, indent + "  "))
        case _ => throw new CompilerError("let values have to be a list")
      }
      case _ => throw new CompilerError("let names have to be a list of strings")
    }
      case "defun" => l(1) match {
        case name: String => l(2) match {
          case parms: List[Any] => 
            val p = parms.map {
              case n: String => n
              case _ => throw new CompilerError("parm names have to be strings")
            }
            val args = p.map(n => isFunction(n, l(3))).zip(p).zipWithIndex.map {
              case ((-1, name: String), _) => 
                name + (usedInFunction(name, l(3)) match {
                  case Some(UsedIn(name, pos)) => 
                    if(higher_order_funs.contains(name))
                      ": (%s) => Any".format(
                      (0 until higher_order_funs(name)(pos)).map(_ => "Any").mkString(", ") )
                    else
                      ": Any"
                  case _ => ": Any"
                })
              case ((n: Int, pname: String), i: Int) => 
                val m = higher_order_funs(name)
                m(i) = n
                higher_order_funs(name) = m
                pname + ": (%s) => Any".format(
                  (0 until n).map(_ => "Any").mkString(", ") )
            }
            global_functions = ("def %s(%s): Any = {\n" + indent + "  %s\n" + indent + "}").format(name, 
                         args.mkString(", "), 
                         process(l(3), indent + "  ")) :: global_functions
            ""
          case _ => throw new CompilerError("function arguments have to be a list")
        }
        case _ => throw new CompilerError("function name has to be a string")
      }

      // function call
      // replace simple functions by operators for prettier code
      case op: String if "+-*/%".contains(op) => l.tail.map(e => process(e)).mkString("(", " %s ".format(op), ")")
      case "append" => l.tail.map(e => process(e)).mkString(" ++ ")
      case "cons" => l.tail.map(e => process(e)).mkString(" :: ")
      case "=" if l.length == 3 => l.tail.map(e => process(e)).mkString(" == ")
      case comp: String if List("<", ">", "<=", ">=", "=", "!=").contains(comp) &&
        l.length == 3 => l.tail.map(e => process(e)).mkString(" %s ".format(comp))

      // other functions have to be called normally
      case name: String => "%s(%s)".format(name, l.tail.map(e => process(e)).mkString(", "))
    }

    case op: String if "+-*/%".contains(op) => "_ %s _".format(op)
    case s: String => s
    // basic values
    case n: Long => n.toString + "l"
    case d: Double => d.toString
    case Literal(l) => "\"" + l + "\""
  }

  def stringify(exp: Any): String = exp match {
    case l: List[Any] => l.map(e => stringify(e)).mkString("List(", ", ", ")")

    case s: String => "\"" + s + "\""

    // basic values
    case n: Long => n.toString + "l"
    case d: Double => d.toString
    case Literal(l) => "Literal(\"" + l + "\")"
  }
}
