package Scalisp

import scala.util.parsing.combinator._

class Env(parent: Env) {
	val map = collection.mutable.Map[String, (Seq[Any]) => Any]()

	def update(k: String, v: (Seq[Any]) => Any) { map.get(k) match {
		case Some(v) => map(k) = v
		case None => parent(k) = v
	} }
	def define(k: String, v: (Seq[Any]) => Any) { map(k) = v }
	def apply(k: String): (Seq[Any]) => Any = map.get(k) match {
		case Some(v) => v
		case None => parent match {
			case null => (_) => k + " not found"
			case _ => parent(k)
		}
	}
}

abstract class Expression { def eval(env: Env): (Seq[Any]) => Any }
case class If(cond: Expression, thn: Expression, els: Expression) extends Expression {
	override def eval(env: Env) = cond.eval(env)(List()) match {
		case r: Boolean => if(r) thn.eval(env) else els.eval(env)
		case _ => els.eval(env)
	}
}
case class Set(v: String, exp: Expression) extends Expression {
	override def eval(env: Env) = (_) => { env(v) = exp.eval(env) }
}
case class Define(v: String, exp: Expression) extends Expression {
	override def eval(env: Env) = (_) => { env.define(v, exp.eval(env)) }
}

case class Number(num: Double) extends Expression {
	override def eval(env: Env) = (_) => num
}
case class Text(s: String) extends Expression {
	override def eval(env: Env) = (_) => s
}
case class Variable(name: String) extends Expression {
	override def eval(env: Env) = (_) => env(name)(List())
}

case class Lambda(params: Seq[Variable], body: Expression) extends Expression {
	override def eval(env: Env) = (paramValues: Seq[Any]) => {
			val context = new Env(env)
			params.zip(paramValues).foreach {
				case (param, value) => context.define(param.name, (_) => value)
			}
			body.eval(context)(List())
		} 
}

case class Procedure(name: Variable, params: Seq[Expression]) extends Expression {
	override def eval(env: Env) = {
		val f = env(name.name) 
		val p = params.map(_.eval(env)(List()))
		(_) => f(p)
	}
}

case class ExpressionList(exps: Seq[Expression]) extends Expression {
	override def eval(env: Env) = (_) => exps map { _.eval(env)(List()) } last
}

object Functions {
	def compare(
		op: (Double, Double) => Boolean, 
		init: Double, 
		n: Seq[Double]
		): Boolean = n.fold( (init, true) ) { 
			case ( (prev: Double, valid: Boolean), cur: Double) => 
				if(valid && op(prev, cur)) (cur, true) else (cur, false) 
			} match {
				case (_, flag: Boolean) => flag
			}

	def op(fun: Seq[Double] => Any) = (nums: Seq[Any]) => nums match {
		case n: Seq[Double] => fun(n)
	}
}

class LispParser extends JavaTokenParsers {
	val defaultEnv = new Env(null) { override val map = collection.mutable.Map[String, (Seq[Any]) => Any]( 
		"+" -> Functions.op(_.reduce(_ + _)), 
		"-" -> Functions.op(_.reduce(_ - _)),
		"*" -> Functions.op(_.reduce(_ * _)),
		"true" -> ((_) => true),
		"false" -> ((_) => false),
		"<" -> Functions.op(n => Functions.compare(_ < _, n.head-1, n)),
		">" -> Functions.op(n => Functions.compare(_ > _, n.head+1, n)),
		"=" -> Functions.op(n => Functions.compare(_ == _, n.head, n))
		) 
	}

	def expression: Parser[Expression] = (
		"("~>expressionBody<~")" | number | string | variable )
	def expressionBody: Parser[Expression] = ( "quote"~>expression
		| "if"~expression~expression~expression ^^ { 
			case "if"~cond~thn~els => If(cond, thn, els) }
		| "set!"~variable~expression ^^ { case "set!"~v~e => Set(v.name, e) }
		| "define"~variable~expression ^^ { case "define"~v~e => Define(v.name, e) }
		| "lambda"~"("~rep(variable)~")"~expression ^^ {
			case "lambda"~"("~args~")"~exp => Lambda(args, exp) }
		| "begin"~rep(expression) ^^ { case "begin"~exps => ExpressionList(exps) }
		| variable~rep(expression) ^^ { case name~params => Procedure(name, params)})
	def number: Parser[Number] = floatingPointNumber ^^ (n => Number(n.toDouble))
	def string: Parser[Text] = "\""~>"[^\"]*".r<~"\"" ^^ (s => Text(s))
	def variable: Parser[Variable] = """[A-Za-z\+\-\<\>\=\*][A-Za-z0-9_]*""".r ^^ (n => Variable(n))

	def executeLine(line: String) = parseAll(expression, line).get.eval(defaultEnv)(List())
}

object REPL extends App {
	val parser = new LispParser()
	Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(line => println( parser.executeLine(line) ))
}