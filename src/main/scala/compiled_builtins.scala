
class TypeError(s: String) extends Exception(s) { }

class MySeq[T](l: Seq[T]) {
  def toDouble = try {
      l.map {
        case l: Long => l.toDouble
        case d: Double => d
      }
    } catch {
      case e: MatchError => throw new TypeError("couldn't convert to double: " + e)
    }

  def toLong = try {
      l.map {
        case l: Long => l
        case d: Double => d.toLong
      }
    } catch {
      case e: MatchError => throw new TypeError("couldn't convert to long: " + e)
    }

  def allLong = l.forall {
    case l: Long => true
    case _ => false
  }
}

import Helper._
object Helper {
  implicit def Seq2MySeq[T](l: Seq[T]) = new MySeq[T](l)
  implicit def Any2MyAny(a: Any) = new MyAny(a)
  implicit def Any2Boolean(a: Any) = a match {
    case b: Boolean => b
    case _ => throw new TypeError("Expected boolean")
  }
}

class MyAny(a: Any) {
  import CompiledBuiltins._
  def <=(that: Any) = compare(_ <= _, -1, List(a, that))
  def >(that: Any) = compare(_ > _, -1, List(a, that))
  def <(that: Any) = compare(_ < _, -1, List(a, that))
  def -(that: Any) = {
    val l = List(a, that)
    if(l.allLong) opL(l, _ - _) else  opD(l, _ - _)
  }
  def +(that: Any) = {
    val l = List(a, that)
    if(l.allLong) opL(l, _ + _) else  opD(l, _ + _)
  }
  def /(that: Any) = {
    val l = List(a, that)
    if(l.allLong) opL(l, _ / _) else  opD(l, _ / _)
  }
  def *(that: Any) = {
    val l = List(a, that)
    if(l.allLong) opL(l, _ * _) else  opD(l, _ * _)
  }
  def ::(that: Any): List[Any] = a match {
    case l: List[Any] => that :: l
    case _ => throw new TypeError("can only cons to list: %s, %s".format(a, that))
  }
  def ++(that: Any) = a match {
    case l: List[Any] => that match {
      case m: List[Any] => l ++ m
      case _ => throw new TypeError("can only append lists")
    }
    case _ => throw new TypeError("can only append lists")
  }

}

case class Literal(s: String)

object CompiledBuiltins {
  // builtin functions
  def compare(
    op: (Double, Double) => Boolean, 
    initOffset: Double, 
    n: Seq[Any]
    ): Boolean = {
    val xs = n.toDouble
    xs.fold( (xs(0) + initOffset, true) ) {
      case ( (prev: Double, valid: Boolean), cur: Double) =>
        if(valid && op(prev, cur)) (cur, true) else (cur, false)
    } match { case (_, flag: Boolean) => flag }
  }

  def opD(l: List[Any], f: (Double, Double) => Double) = {
    l.toDouble.reduce(f)
  }

  def opL(l: List[Any], f: (Long, Long) => Long) = {
    l.toLong.reduce(f)
  }

  def print(args: Any*) = println(args.mkString)
  def <(args: Any*) = compare(_ < _, -1, args)
  def <=(args: Any*) = compare(_ <= _, -1, args)
  def car(l: Any) = l match {
    case list: List[Any] => list.head
    case _ => throw new TypeError("Expected list")
  }
  def cdr(l: Any) = l match {
    case list: List[Any] => list.tail
    case _ => throw new TypeError("Expected list")
  }
  def list(args: Any*) = args
  def shuffle(l: Any) = l match {
    case list: List[Any] => util.Random.shuffle(list)
    case _ => throw new TypeError("Expected list")
  }

  // user methods
    def filter(f: (Any) => Any, seq: Any): Any = {
    if(List() == seq) {
      List()
    } else {
      if(f(car(seq))) {
        car(seq) :: filter(f, cdr(seq))
      } else {
        filter(f, cdr(seq))
      }
    }
  }

  def reduce(f: (Any, Any) => Any, seq: Any): Any = {
    foldl(f, car(seq), cdr(seq))
  }

  def foldl(f: (Any, Any) => Any, acc: Any, seq: Any): Any = {
    if(List() == seq) {
      acc
    } else {
      foldl(f, f(acc, car(seq)), cdr(seq))
    }
  }

  def map(f: (Any) => Any, seq: Any): Any = {
    if(List() == seq) {
      List()
    } else {
      f(car(seq)) :: map(f, cdr(seq))
    }
  }

  def range(start: Any, stop: Any): Any = {
    if(stop <= start) {
      List()
    } else {
      start :: range(start + 1l, stop)
    }
  }

  def range(stop: Any): Any = {
    range(0l, stop)
  }

  def subseq(list: Any, start: Any, stop: Any): Any = {
    if(start > 0l) {
      subseq(subseq(list, start), 0l, stop - start)
    } else {
      if(stop <= 0l) {
        List()
      } else {
        car(list) :: subseq(cdr(list), 0l, stop - 1l)
      }
    }
  }

  def subseq(list: Any, start: Any): Any = {
    if(start <= 0l) {
      list
    } else {
      subseq(cdr(list), start - 1l)
    }
  }

  def reverse(l: Any): Any = {
    if(List() == l) {
      l
    } else {
      reverse(cdr(l)) ++ list(car(l))
    }
  }

  def length(list: Any): Any = {
    if(list == List()) {
      0l
    } else {
      length(cdr(list)) + 1l
    }
  }


}