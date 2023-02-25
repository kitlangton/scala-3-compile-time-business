package lab.macros
import lab.custom
import lab.pages.*

import scala.quoted.*
import pprint.*

import scala.compiletime.*

inline def showType[A]: String = ${ showTypeImpl[A] }
inline def printType[A]: Unit  = println(showType[A])

inline def debug[A](inline expr: A): A       = ${ debugImpl('expr) }
inline def debugInline[A](inline expr: A): A = ${ debugInlineImpl('expr) }

// drop first and last lines
// strip preceding indentation
def processString(str: String): String =
  val lines  = str.split("\n").drop(1).init
  val indent = lines.map(_.takeWhile(_ == ' ').length).min
  lines.map(_.drop(indent)).mkString("\n")

val sepLength = 90

def debugImpl[A](expr: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*
  val location = Position.ofMacroExpansion
  val fileName = location.sourceFile.name + ":" + location.startLine
  val tpe      = expr.asTerm.tpe.show
  println()
  println(s"${fileName} ${"–" * (sepLength - fileName.length)}".green.dim)
  println(s"${expr.show}".green)
  println(s"${("–" * (sepLength - tpe.length))} $tpe".green.dim)
  println()
  expr

def debugInlineImpl[A](expr: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*
  val location = Position.ofMacroExpansion
  val fileName = location.sourceFile.name + ":" + location.startLine
  val tpe      = expr.asTerm.tpe.show
  println()
  println(s"${fileName} ${"–" * (sepLength - fileName.length)}".green.dim)
  println(processString(location.sourceCode.get).green)
  println(("–" * (sepLength + 1)).green.dim)
  println(s"${expr.show}".green)
  println(s"${("–" * (sepLength - tpe.length))} $tpe".green.dim)
  println()
  expr

inline def typeRepr[A]: String    = ${ showTypeReprImpl[A] }
inline def printTypeRepr[A]: Unit = println(typeRepr[A])

inline def customRepr[A]: custom.TypeRepr = ${ getReprImpl[A] }

def showTypeImpl[A: Type](using Quotes) =
  import quotes.reflect.*
  val typeString = TypeRepr.of[A].widen.show
  Expr(typeString)

def showTypeReprImpl[A: Type](using Quotes) =
  import quotes.reflect.*
  val repr = TypeRepr.of[A]
  pprintln(repr)
  Expr(repr.toString)

def getReprImpl[A: Type](using Quotes): Expr[custom.TypeRepr] =
  import quotes.reflect.*
  val repr       = TypeRepr.of[A]
  val customRepr = custom.TypeRepr.fromTypeRepr(repr.dealias)
  Expr(customRepr)
