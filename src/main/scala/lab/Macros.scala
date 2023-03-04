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
def debugImpl[A: Type](expr0: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*
  val location  = Position.ofMacroExpansion
  val fileName0 = location.sourceFile.name + ":" + location.startLine
  val tpe0      = expr0.asTerm.tpe.show
  '{
    val fileName = ${ Expr(fileName0) }
    val tpe      = ${ Expr(tpe0) }
    val expr     = ${ Expr(expr0.show) }
    println()
    println(s"${fileName} ${"–" * (sepLength - fileName.length)}".green.dim)
    println(s"${expr}".green)
    println(s"${("–" * (sepLength - tpe.length))} $tpe".green.dim)
    val res = $expr0
    println(s"${res}".green)
    println(s"${("–" * (sepLength + 1))}".green.dim)
    println()
    res
  }

def debugInlineImpl[A: Type](expr0: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*
  val location  = Position.ofMacroExpansion
  val fileName0 = location.sourceFile.name + ":" + location.startLine
  val source    = location.sourceCode.get
  val tpe0      = expr0.asTerm.tpe.show
  '{
    val fileName = ${ Expr(fileName0) }
    val tpe      = ${ Expr(tpe0) }
    val expr     = ${ Expr(expr0.show) }
    println()
    println(s"${fileName} ${"–" * (sepLength - fileName.length)}".green.dim)
    println(processString(${ Expr(source) }).green)
    println(("–" * (sepLength + 1)).green.dim)
    println(s"${expr}".green)
    println(s"${("–" * (sepLength - tpe.length))} $tpe".green.dim)
    val res = $expr0
    println(s"${res}".green)
    println(s"${("–" * (sepLength + 1))}".green.dim)
    println()
    res
  }

inline def typeRepr[A]: String    = ${ showTypeReprImpl[A] }
inline def printTypeRepr[A]: Unit = println(typeRepr[A])

inline def customRepr[A]: custom.TypeRepr = ${ getReprImpl[A] }

def showTypeImpl[A: Type](using Quotes): Expr[String] =
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

trait Cool:
  type Nice

inline def refine[A]: Unit = ${ refineImpl[A] }

def refineImpl[A: Type](using Quotes) =
  import quotes.reflect.*

  val symbol = TypeTree.of[A].symbol
  symbol.caseFields

  val ap = TypeRepr.typeConstructorOf((1 *: EmptyTuple).getClass())

  val pairType = TypeRepr.of[*:[?, ?]] match
    case AppliedType(t, List(a, b)) => t

  val emptyTupleType = TypeRepr.of[EmptyTuple]

  val elemLabels =
    symbol.caseFields
      .map(field => ConstantType(StringConstant(field.name)))
      .foldRight(TypeRepr.of[EmptyTuple]) { (elem, acc) => //
        AppliedType(pairType, List(elem, acc))
      }

  // Type.valueOfTuple

  val repr = TypeRepr.of["title" *: "author" *: "year" *: EmptyTuple]
  // val repr2 = Refinement(TypeRepr.of[Cool], "Nice", TypeBounds(TypeRepr.of[A], TypeRepr.of[A]))
  report.errorAndAbort(s"repr: ${repr.show}\nrepr2: ${elemLabels.show}")

trait ORM[A]:
  transparent inline def hasMany[B](inline select: B => Any, inline bs: List[B]): HasMany[A, B, ?] =
    ${ ORMImpl.hasManyImpl[A, B]('select, 'bs) }

object ORMImpl:
  def hasManyImpl[A: Type, B: Type](select: Expr[B => Any], bs: Expr[List[B]])(using Quotes): Expr[HasMany[A, B, ?]] =
    import quotes.reflect.*

    // val methodSymbols = MethodInfo.classMethods[A]
    // report.errorAndAbort(s"methods: ${methodSymbols.map(_.toString).mkString("\n")}")

    val aType   = Symbol.of[A]
    val aId     = aType.fieldMember("id")
    val aIdType = aId.returnType

    val selectReturnType = select.asTerm.uninline match
      case Lambda(args, body) =>
        body.tpe.widenTermRefByName

    if !(selectReturnType =:= aIdType) then report.errorAndAbort(s"note: ${aIdType.show} != ${selectReturnType.show}")

    val hasManyExpr =
      aIdType.asType match
        case '[keyType] =>
          val getIdExpr = '{ (a: A) => ${ 'a.asTerm.select(aId).asExprOf[keyType] } }
          '{ HasMany[A, B, keyType](${ getIdExpr }, ${ select }.asInstanceOf[B => keyType], $bs) }

    // report.errorAndAbort(s"note: ${selectReturnType.show}")
    hasManyExpr
// ???

trait Relation[A, B]:
  def apply(a: A): B

  def zip[C](that: Relation[A, C]): Relation[A, (B, C)] =
    Zip(this, that)

case class HasMany[A, B, Key](getId: A => Key, getForeignKey: B => Key, bs: List[B]) extends Relation[A, List[B]]:
  def apply(a: A): List[B] =
    val aId = getId(a)
    bs.filter(b => getForeignKey(b) == aId)

case class Zip[A, Out1, Out2](left: Relation[A, Out1], right: Relation[A, Out2]) extends Relation[A, (Out1, Out2)]:
  def apply(a: A): (Out1, Out2) =
    left(a) -> right(a)

// Extensions

extension (using Quotes)(symbol: quotes.reflect.Symbol.type)
  def of[A: Type]: quotes.reflect.Symbol =
    import quotes.reflect.*
    TypeTree.of[A].symbol

extension (using Quotes)(symbol: quotes.reflect.Symbol)
  def returnType: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    symbol.termRef.widenTermRefByName

  def isPublic: Boolean =
    import quotes.reflect.*
    !symbol.flags.is(Flags.Private) && !symbol.flags.is(Flags.Protected) &&
    !symbol.flags.is(Flags.Local) && !symbol.flags.is(Flags.Synthetic) &&
    !symbol.flags.is(Flags.Artifact) && !symbol.flags.is(Flags.Macro)

extension (using Quotes)(symbol: quotes.reflect.Term)
  def uninline: quotes.reflect.Term =
    import quotes.reflect.*
    symbol match
      case Inlined(_, _, term) => term.uninline
      case _                   => symbol

extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
  def typeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    tpe.asType match
      case '[t] => TypeTree.of[t]
