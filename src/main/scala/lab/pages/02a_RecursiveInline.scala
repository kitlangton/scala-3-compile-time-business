package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

import lab.pages.{Config, Logger}

// ================================================================================
// Recursive Inline
// ================================================================================

inline def power(x: Double, n: Int): Double =
  inline if n == 0 then 1.0
  else inline if n == 1 then x
  else
    val y = power(x, n / 2)
    inline if n % 2 == 0 then y * y else y * y * x

// Mutually Recursive Inline
@main
def inlineRecursionExample =
  println {
    debugInline {
      power(10, 2)
    }
  }
