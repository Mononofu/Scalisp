package Scalisp

class InterpreterException(s: String) extends Exception(s) { }
class VariableNotFound(s: String) extends InterpreterException(s) { }
class MethodNotFound(s: String) extends InterpreterException(s) { }
class InvalidName(s: String) extends InterpreterException(s) { }
class TypeError(s: String) extends InterpreterException(s) { }

case class Function(parms: List[String], body: Any) {
  def arity = parms.length
}

class FunctionTable() {
  val fs = collection.mutable.Map[Int, Function]()
  def add(f: Function) { fs(f.arity) = f }
  def apply(arity: Int): Option[Function] = fs.get(arity)

  override def toString = "Function table: " + fs.mkString("\n")
}

object Interpreter {
  def eval(expression: Any, environment: Env): Any = {
    var exp = expression
    var env = environment
    while(true) {
      exp match {
        case l: List[Any] => l.head match {
          // special forms
          case "if" => eval(l(1), env) match {
            case true => exp = l(2)
            case false => exp = l(3)
          }
          case "define" => return l(1) match {
            case name: String => env.define(name, eval(l(2), env))
            case _ => throw new InvalidName("variable name has to be a string")
          }
          case "set!" => return l(1) match {
            case name: String => env.update(name, eval(l(2), env))
            case _ => throw new InvalidName("variable name has to be a string")
          }
          case "begin" => 
            l.tail.init.map(e => eval(e, env))
            exp = l.last
          case "quote" => return l(1)
          case "lambda" => return l(1) match {
            case parms: List[Any] => 
              val p = parms.map {
                case n: String => n
                case _ => "parm names have to be strings"
              }
              Function(p, l(2))
            case _ => throw new InterpreterException("lambda arguments have to be a list")
          }
          case "defun" => return l(1) match {
            case name: String => l(2) match {
              case parms: List[Any] => 
                val p = parms.map {
                  case n: String => n
                  case _ => "parm names have to be strings"
                }
                if(l.length != 4) throw new InterpreterException("function has to have form (defun <name> <parms> <bod>)")
                val f = Function(p, l(3))
                env.define(name, f)
              case _ => throw new InterpreterException("function arguments have to be a list")
            }
            case _ => throw new InvalidName("function name has to be a string")
          }
          case "let" => return l(1) match {
            case names: List[String] => l(2) match {
              case vals: List[Any] => 
                val body = l(3)
                val context = new Env(env)
                val args = vals.map(e => eval(e, env))
                names.zip(args).foreach {
                  case (param: String, value: Any) => context.define(param, value)
                }
                eval(body, context)
              case _ => throw new InterpreterException("let values have to be a list")
            }
            case _ => throw new InterpreterException("let names have to be a list of strings")
          }
          case n: String if Builtins.builtins(l, env).isDefinedAt(n) => 
            return Builtins.builtins(l, env)(n)
          case s: String => env.getFunction(s, l.tail.length) match {
            case None => 
              val b = Builtins.builtins(l, env)
              val fname = env(s) match {
                case Some(s: String) => s
                case _ => throw new MethodNotFound(s)
              }
              if( b.isDefinedAt(fname)) {
                return b(fname)
              } else {
                throw new MethodNotFound(s)
              }
            case Some(Function(parms, body)) => 
              val context = new Env(env)
              val args = l.tail.map(e => eval(e, env))
              parms.zip(args).foreach {
                case (param: String, value: Any) => context.define(param, value)
              }
              exp = body
              env = context
            }

          case _ => throw new TypeError("can't call non-string function")
        }

        case s: String => return env(s) match {
          case None => throw new VariableNotFound(s)
          case Some(v) => v
        }

        // basic values
        case n: Long => return n
        case d: Double => return d
        case Literal(l) => return l
      }
    }
  }
}