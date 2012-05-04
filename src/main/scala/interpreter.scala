package Scalisp

class InterpreterException(s: String) extends Exception(s) { }
class VariableNotFound(s: String) extends InterpreterException(s) { }
class MethodNotFound(s: String) extends InterpreterException(s) { }
class InvalidName(s: String) extends InterpreterException(s) { }
class TypeError(s: String) extends InterpreterException(s) { }

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
      case parms: List[Any] => (parms, l(2))
      case _ => throw new InterpreterException("lambda arguments have to be a list")
    }
  }

  def userFunctions(l: List[Any], env: Env): PartialFunction[String, Any] = {
    // eval user functions
    case s: String => env(s) match {
      case None => throw new MethodNotFound(s)
      case Some(m) => 
        val context = new Env(env)
        val (parms, body) = m
        val args = l.tail.map(e => eval(e, env))
        parms match {
          case p: List[Any] => p.zip(args).foreach {
            case (param: String, value) => context.define(param, value)
          }
          case _ => throw new TypeError("function params have to be a list")
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