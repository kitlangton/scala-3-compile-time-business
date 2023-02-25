package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

// ================================================================================
// Const Value
// ================================================================================

@main
def constValueExample =
  // String, Int, Boolean
  // val number: 12 = 12
  // val b: true    = true
  val x: 12  = 12
  val result = constValue[x.type]
  println(result.toString.red)
