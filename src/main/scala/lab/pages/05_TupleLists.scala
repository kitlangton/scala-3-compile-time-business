package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

// ================================================================================
// The All New Scala Tuple 2023
// ================================================================================

// ::(10, ::(20, Nil))
// TODO: Tuple from scratch!

// val tuple1: (String, Int, Boolean, Double) = "hello" *: 1 *: true *: 3.5 *: EmptyTuple
// val tuple2: (String, Int, Boolean, Double) = ("hello", 1, true, 3.5)
// val tuple3: ("hello", 1, true, 3.5)        = ("hello", 1, true, 3.5)

// trait Tuple
// H *: T
// EmptyTuple
transparent inline def myConstValueTuple[T <: Tuple]: T = {
  inline erasedValue[T] match
    case _: (h *: t)   => constValue[h] *: myConstValueTuple[t]
    case _: EmptyTuple => EmptyTuple
}.asInstanceOf[T]

def myConstValueTupleReal[T <: Tuple](tuple: T): T = {
  tuple match
    case (h *: t)   => h *: myConstValueTupleReal(t)
    case EmptyTuple => EmptyTuple
}.asInstanceOf[T]

// Mirror -> derive implementations of type classes

@main
def tupleExample =
  val result   = debug(myConstValueTuple["hello" *: 12 *: EmptyTuple])
  val recreate = myConstValueTupleReal(result)
  println(recreate.toString.red)
