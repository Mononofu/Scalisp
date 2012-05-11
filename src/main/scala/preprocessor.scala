package Scalisp

object Preprocessor {
  val macros = collection.mutable.Map[String, Function]()

  def process(ast: List[Any]) = {
    val (mac, code) = ast.partition {
      case "defmacro" :: body => true
      case _ => false
    }
    mac.foreach {
      case List("defmacro", name: String, args: List[String], body) =>
        macros(name) = Function(args, body) 
    }
    replaceMacros(code) match {
      case l: List[Any] => l
    }
  }

  def replaceMacros(ast: Any): Any = ast match {
    case (name: String) :: args if macros.contains(name) => applyMacro(name, args)
    case l: List[Any] => l.map(e => replaceMacros(e))
    case e => e
  }

  def applyMacro(name: String, args: List[Any]): Any = {
    val m = macros(name)
    val pars = m.parms.zip(args).map {
      case (name, value) => name -> value
    }
    applyMacroRec(m.body, pars.toMap)
  }

  def applyMacroRec(ast: Any, args: Map[String, Any]): Any = ast match {
    case Replace(n) => args(n)
    case l: List[Any] => l.map(e => applyMacroRec(e, args))
    case e => e
  }
}