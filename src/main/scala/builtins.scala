package Scalisp

class MySeq[T](l: Seq[T]) {
  def toNumeric = try {
      l.map {
        case l: Long => l.toDouble
        case d: Double => d
      }
    } catch {
      case e: MatchError => throw new TypeError("couldn't convert to numeric: " + e)
    }
}

object Helper {
  implicit def Seq2MySeq[T](l: Seq[T]) = new MySeq[T](l)
}

import Helper._

object Builtins {
    def compare(
    op: (Double, Double) => Boolean, 
    initOffset: Double, 
    n: Seq[Any],
    env: Env
    ): Boolean = {
    val xs = n.map(e => Interpreter.eval(e, env)).toNumeric
    xs.fold( (xs(0) + initOffset, true) ) {
      case ( (prev: Double, valid: Boolean), cur: Double) =>
        if(valid && op(prev, cur)) (cur, true) else (cur, false)
    } match { case (_, flag: Boolean) => flag }
  }

  def op(l: List[Any], f: (Double, Double) => Double, env: Env) = {
    l.map(e => Interpreter.eval(e, env)).toNumeric.reduce(f)
  }

  def argCount(l: List[Any], n: Int) {
    if(l.length - 1 != n) throw new InterpreterException(
      "invalid number of arguments: expected %d, got %d".format(n, l.length - 1))
  }

  def builtins(l: List[Any], env: Env): PartialFunction[String, Any] = {
    // arithmetic
    case "+" => op(l.tail, _ + _, env)
    case "*" => op(l.tail, _ * _, env)
    case "-" => op(l.tail, _ - _, env)
    case "/" => op(l.tail, _ / _, env)
    case "%" => op(l.tail, _ % _, env)
    case "min" => l.tail.map(e => Interpreter.eval(e, env)).toNumeric.min
    case "max" => l.tail.map(e => Interpreter.eval(e, env)).toNumeric.max

    // comparisons
    case "<" => compare(_ < _, -1, l.tail, env)
    case ">" => compare(_ > _, 1, l.tail, env)
    case ">=" => compare(_ >= _, 0, l.tail, env)
    case "<=" => compare(_ <= _, 0, l.tail, env)
    case "=" => l.tail.map(e => Interpreter.eval(e, env)).distinct.length == 1
    case "!=" => l.tail.map(e => Interpreter.eval(e, env)).distinct.length > 1

    case "atom" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => false
      case _ => true
    }

    // string furnctions
    case "to-string" => argCount(l, 1); Interpreter.eval(l(1), env).toString
    case "concat" => l.tail.map(e => Interpreter.eval(e, env)).mkString

    // list functions
    case "car" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.head
      case s: String => s.head
      case Literal(s) => s.head
      case _ => throw new TypeError("can't get head of non-list")
    }

    case "cdr" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.tail
      case s: String => s.tail
      case Literal(s) => s.tail
      case _ => throw new TypeError("can't get tail of non-list")
    }

    case "cons" => argCount(l, 2); Interpreter.eval(l(2), env) match {
      case list: List[Any] => Interpreter.eval(l(1), env) :: list
      case _ => throw new TypeError("can't cons to non-list")
    }

    case "append" => l.tail.map(e => Interpreter.eval(e, env)).flatMap {
      case l: List[Any] => l
      case _ => throw new TypeError("can't append non-lists")
    }

    case "list" => l.tail.map(e => Interpreter.eval(e, env))

    case "shuffle" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case list: List[Any] => util.Random.shuffle(list)
      case _ => throw new TypeError("can't shuffle a non-list")
    }

    case "print" => println(l.tail.map(e => Interpreter.eval(e, env)).mkString)
  }
}