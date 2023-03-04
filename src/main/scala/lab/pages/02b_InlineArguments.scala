package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

import lab.pages.{Config, Logger}

// ================================================================================
// Inline Argument Behavior
// ================================================================================

def example(x: => Int) =
  (x, x, x)

inline def useDouble(expected: Double) =
  expected

inline def funkyAssertEquals(actual: Double, expected: => Double, inline delta: Double): Unit =
  actual
  useDouble(expected)

def computeActual: Double   = power(5, 4)
def computeExpected: Double = 623
def computeDelta: Double    = 0.0001

@main
def inlineArgumentExample =
  debugInline {
    funkyAssertEquals(computeActual, computeExpected, computeDelta + 5)
  }
// example { println("getting 5".red); 5 }
