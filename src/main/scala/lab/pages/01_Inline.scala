package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

import lab.pages.{Config, Logger}

// ================================================================================
// Inline
// ================================================================================

// Compile Time Operation
// macro: AST => AST
// inline
// Phases: String => Tokens => Ast => Ast => Ast => Ast => Ast => Jvm Output
inline def inlineBusiness(strategy: String, kpis: Int) =
  999

def doBusiness(strategy: String, kpis: Int) =
  999

@main
def inlineExample =
  debug {
    inlineBusiness("hello", 3)
  }
  debug {
    doBusiness("hello", 3)
  }
