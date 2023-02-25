package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

import lab.pages.{Config, Logger}

// ================================================================================
// Transparent
// ================================================================================

case object BaseballBat
case object FrogPizza

transparent inline def choose(b: Boolean): Any =
  inline if b then BaseballBat else FrogPizza

@main
def transparentExample =
  debugInline {
    choose(true)
  }
  debugInline {
    choose(false)
  }
