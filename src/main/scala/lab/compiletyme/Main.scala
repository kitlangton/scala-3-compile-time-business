package lab.compiletyme

import scala.quoted.*
import pprint.*

inline def myConstValue[A]: A = ${ myConstValueImpl[A] }

def myConstValueImpl[T](using Quotes)(using Type[T]): Expr[T] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  val const = tpe match
    case ConstantType(const) => const
    case other               => report.throwError(s"Must be a constant: $other")

  const.value match
    case value: Int     => Expr(value).asExprOf[T]
    case value: Boolean => Expr(value).asExprOf[T]
    case value: String  => Expr(value).asExprOf[T]
    case value: Double  => Expr(value).asExprOf[T]
    case other          => report.throwError(s"Unexpected constant: $other")
