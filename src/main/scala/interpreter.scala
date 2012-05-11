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

  def specialForms(l: List[Any], env: Env): PartialFunction[String, Any] = {
    // special forms
    case "if" => eval(l(1), env) match {
      case true => eval(l(2), env)
      case false => eval(l(3), env)
    }
    case "define" => l(1) match {
      case name: String => env.define(name, eval(l(2), env))
      case _ => throw new InvalidName("variable name has to be a string")
    }
    case "set!" => l(1) match {
      case name: String => env.update(name, eval(l(2), env))
      case _ => throw new InvalidName("variable name has to be a string")
    }
    case "begin" => l.tail.map(e => eval(e, env)).last
    case "quote" => l(1)
    case "lambda" => l(1) match {
      case parms: List[Any] => 
        val p = parms.map {
          case n: String => n
          case _ => "parm names have to be strings"
        }
        Function(p, l(2))
      case _ => throw new InterpreterException("lambda arguments have to be a list")
    }
    case "defun" => l(1) match {
      case name: String => l(2) match {
        case parms: List[Any] => 
          val p = parms.map {
            case n: String => n
            case _ => "parm names have to be strings"
          }
          val f = Function(p, l(3))
          env.define(name, f)
        case _ => throw new InterpreterException("function arguments have to be a list")
      }
      case _ => throw new InvalidName("function name has to be a string")
    }
  }

  def userFunctions(l: List[Any], env: Env): PartialFunction[String, Any] = {
    // eval user functions
    case s: String => env.getFunction(s, l.tail.length) match {
      case None => 
        val b = Builtins.builtins(l, env)
        val fname = env(s) match {
          case Some(s: String) => s
          case _ => throw new MethodNotFound(s)
        }
        if( b.isDefinedAt(fname)) {
          b(fname)
        } else {
          throw new MethodNotFound(s)
        }
      case Some(Function(parms, body)) => 
        val context = new Env(env)
        val args = l.tail.map(e => eval(e, env))
        parms.zip(args).foreach {
          case (param: String, value: Any) => context.define(param, value)
        }
        eval(body, context)
      }
  }

  def eval(exp: Any, env: Env): Any = exp match {
    case l: List[Any] => l.head match {
      case n: String => (specialForms(l, env) orElse Builtins.builtins(l, env) orElse userFunctions(l, env))(n)
      case _ => throw new TypeError("can't call non-string function")
    }

    case s: String => env(s) match {
      case None => throw new VariableNotFound(s)
      case Some(v) => v
    }

    // basic values
    case n: Long => n
    case d: Double => d
    case Literal(l) => l
  }
}