package Scalisp

import scala.util.parsing.combinator._

class Env(m: collection.mutable.Map[String, (Any*) => Any]) {
	def update(k: String, v: Any) = m(k) = ( (args: Any) => v)
	def apply(k: String) = m(k)
}

abstract class Expression { def eval(env: Env): Any }
case class If(cond: Expression, thn: Expression, els: Expression) extends Expression {
	override def eval(env: Env) = cond.eval(env) match {
		case r: Boolean => if(r) thn.eval(env) else els.eval(env)
		case _ => els.eval(env)
	}
}
case class Set(v: String, exp: Expression) extends Expression {
	override def eval(env: Env) { env(v) = exp.eval(env) }
}
case class Define(v: String, exp: Expression) extends Expression {
	override def eval(env: Env) { env(v) = exp.eval(env) }
}


case class Number(num: Int) extends Expression {
	override def eval(env: Env): Int = num
}
case class Variable(name: String) extends Expression {
	override def eval(env: Env): (Any*) => Any = env(name)
}

case class Lambda(params: List[Variable], body: Expression) extends Expression {
	override def eval(env: Env) {
	}
}
case class Procedure(name: Variable, params: List[Expression]) extends Expression {
	override def eval(env: Env) = {
		val f = name.eval(env) 
		val p = params.map(_.eval(env))
		f(p: _*)
	}
}

case class ExpressionList(exps: List[Expression]) extends Expression {
	override def eval(env: Env) = exps map { _.eval(env) } last
}

object Functions {
	def add(nums: Any*) = nums match {
		case n: Seq[Int] => n.reduce(_ + _)
		case s: Seq[String] => s.mkString
	}
}

class LispParser extends JavaTokenParsers {
	val defaultEnv = new Env(collection.mutable.Map[String, (Any*) => Any]( ("add", Functions.add )))

	def expression: Parser[Expression] = ( 
		"("~>expressionBody<~")" |
		number |
		variable )
	def expressionBody: Parser[Expression] = (
		"quote"~>expression
		| "if"~expression~expression~expression ^^ {
			case "if"~cond~thn~els => If(cond, thn, els)
		}
		| "set!"~variable~expression ^^ {
			case "set!"~v~e => Set(v.name, e)
		}
		| "define"~variable~expression ^^ {
			case "define"~v~e => Define(v.name, e)
		}
		| "lambda"~"("~repsep(variable, " ")~")"~expression ^^ {
			case "lambda"~"("~args~")"~exp => Lambda(args, exp)
		}
		| "begin"~repsep(expression, " ") ^^ {
			case "begin"~exps => ExpressionList(exps)			
		}
		| variable~rep(expression) ^^ {
			case name~params => Procedure(name, params)
		} )
	def number: Parser[Number] = """\d+""".r ^^ (n => Number(n.toInt)) 
	def variable: Parser[Variable] = "[A-Za-z][A-Za-z0-9_]*".r ^^ (n => Variable(n))

	def executeLine(line: String) = parseAll(expression, line).get.eval(defaultEnv)

	override def skipWhitespace = true
}

object REPL extends App {
	val parser = new LispParser()

	Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(line => println( parser.executeLine(line) ))

}