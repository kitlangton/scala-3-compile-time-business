package lab.pages

import scala.compiletime.*
import scala.deriving.*
import pprint.*
import lab.macros.*

trait Show[A]:
  def show(a: A): String

given Show[String] with
  def show(a: String) = a.green

given Show[Int] with
  def show(a: Int) = a.toString.blue

given Show[Boolean] with
  def show(a: Boolean) = a.toString.cyan

given Show[Double] with
  def show(a: Double) = a.toString.cyan

case class SSN(value: String)

object SSN:
  given Show[SSN] with
    def show(a: SSN) = "SSN(REDACTED)".red

// ================================================================================
// Mirrors
// ================================================================================

// inline def summonShowInstances[T <: Tuple]: List[Show[?]] =
//   inline erasedValue[T] match
//     case _: (h *: t)   => summonInline[Show[h]] :: summonShowInstances[t]
//     case _: EmptyTuple => Nil

// inline given [A](using mirror: Mirror.ProductOf[A]): Show[A] = Show.derived[A]

object Show:
  inline def derived[A](using mirror: Mirror.ProductOf[A]) =
    // -> -> -> kitlangton/quotidian
    // MacroMirror
    // mirror.construct()
    // User(name, age, int)
    new Show[A]:
      def show(value: A) =
        val classLabel           = constValue[mirror.MirroredLabel]
        val labels: List[String] = constValueTuple[mirror.MirroredElemLabels].toList.asInstanceOf[List[String]]
        val values: List[Any]    = value.asInstanceOf[Product].productIterator.toList
        val shows: List[Show[Any]] =
          summonAll[Tuple.Map[mirror.MirroredElemTypes, Show]].toList.asInstanceOf[List[Show[Any]]]
        val labelString =
          labels
            .zip(values)
            .zip(shows)
            .map { case ((l, v), show) =>
              s"$l = ${show.show(v)}"
            }
            .mkString(", ")
        s"$classLabel($labelString)"

final case class User(name: String, age: Int, isAlive: Boolean, ssn: SSN, more: Double, pet: Pet) derives Show
object User

case class Pet(name: String, numberOfSwords: Int, ssn: SSN) derives Show

@main
def mirrorExample() =
  val mirror = summon[Mirror.Of[User]]
  val pet    = Pet("Jimmy The Pet", 55, SSN("Woofwoofpetbark"))
  val user   = User("Kit", 32, false, SSN("55566688"), 55.55, pet)
  val shown  = summon[Show[User]].show(user)
  println(shown)
