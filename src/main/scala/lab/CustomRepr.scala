package lab.custom

import scala.quoted as q
import scala.quoted.{ToExpr, Quotes, Expr, quotes}

enum TypeRepr:
  case TypeRef(qual: TypeRepr, name: String)
  case TermRef(qual: TypeRepr, name: String)
  case ThisType(tref: TypeRepr)
  case AppliedType(tycon: TypeRepr, args: List[TypeRepr])
  case TypeBounds(lo: TypeRepr, hi: TypeRepr)
  case NoPrefix
  case ConstantType(constant: Constant)

object TypeRepr:

  given ToExpr[TypeRepr] = new ToExpr[TypeRepr]:
    def apply(x: TypeRepr)(using Quotes): Expr[TypeRepr] = x match
      case TypeRef(qual, name)      => '{ TypeRef(${ Expr(qual) }, ${ Expr(name) }) }
      case TermRef(qual, name)      => '{ TermRef(${ Expr(qual) }, ${ Expr(name) }) }
      case ThisType(tref)           => '{ ThisType(${ Expr(tref) }) }
      case NoPrefix                 => '{ NoPrefix }
      case AppliedType(tycon, args) => '{ AppliedType(${ Expr(tycon) }, ${ Expr(args) }) }
      case TypeBounds(lo, hi)       => '{ TypeBounds(${ Expr(lo) }, ${ Expr(hi) }) }
      case ConstantType(constant)   => '{ ConstantType(${ Expr(constant) }) }

  def fromTypeRepr(using Quotes)(repr: quotes.reflect.TypeRepr): TypeRepr =
    import quotes.reflect as r
    repr match
      case r.TypeRef(qual, name)      => TypeRef(fromTypeRepr(qual), name)
      case r.TermRef(qual, name)      => TermRef(fromTypeRepr(qual), name)
      case r.ThisType(tref)           => ThisType(fromTypeRepr(tref))
      case r.NoPrefix()               => NoPrefix
      case r.AppliedType(tycon, args) => AppliedType(fromTypeRepr(tycon), args.map(fromTypeRepr))
      case r.TypeBounds(lo, hi)       => TypeBounds(fromTypeRepr(lo), fromTypeRepr(hi))
      case r.ConstantType(constant)   => ConstantType(Constant.fromConstant(constant))
      case other                      => r.report.throwError(s"Unexpected type: $other")

enum Constant:
  case CInt(value: Int)
  case CBoolean(value: Boolean)
  case CString(value: String)

object Constant:
  given ToExpr[Constant] = new ToExpr[Constant]:
    def apply(x: Constant)(using Quotes): Expr[Constant] = x match
      case CInt(value)     => '{ CInt(${ Expr(value) }) }
      case CBoolean(value) => '{ CBoolean(${ Expr(value) }) }
      case CString(value)  => '{ CString(${ Expr(value) }) }

  def fromConstant(using Quotes)(constant: quotes.reflect.Constant): Constant =
    import quotes.reflect as r
    constant.value match
      case value: Int     => CInt(value)
      case value: Boolean => CBoolean(value)
      case value: String  => CString(value)
      case other          => r.report.throwError(s"Unexpected constant: $other")
