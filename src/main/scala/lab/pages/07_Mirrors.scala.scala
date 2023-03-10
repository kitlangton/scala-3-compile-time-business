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

trait CaseClass[A, TypeClass[_]]:
  def label: String
  def params: List[Param[A, TypeClass]]

trait Param[A, TypeClass[_]]:
  type PType

  def label: String
  def typeclass: TypeClass[PType]

  // nameParam.dereference(person) -> person.name
  def dereference(a: A): PType

trait Derivation[TypeClass[_]]:
  def join[A](caseClass: CaseClass[A, TypeClass]): TypeClass[A]

  inline def derived[A](using mirror: Mirror.ProductOf[A]): TypeClass[A] =
    val classLabel: String = constValue[mirror.MirroredLabel]

    val labels: List[String] =
      constValueTuple[mirror.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val typeclasses: List[TypeClass[Any]] =
      summonAll[Tuple.Map[mirror.MirroredElemTypes, TypeClass]].toList.asInstanceOf[List[TypeClass[Any]]]

    val params0: List[Param[A, TypeClass]] =
      labels.zip(typeclasses).zipWithIndex.map { case ((label0, typeclass0), idx) =>
        new Param[A, TypeClass]:
          def label: String               = label0
          def typeclass: TypeClass[PType] = typeclass0.asInstanceOf[TypeClass[PType]]
          def dereference(a: A): PType    = a.asInstanceOf[Product].productElement(idx).asInstanceOf[PType]
      }

    lazy val caseClass: CaseClass[A, TypeClass] =
      new CaseClass:
        val label: String                     = classLabel
        val params: List[Param[A, TypeClass]] = params0

    join[A](caseClass)

object Show extends Derivation[Show]:

  // A = User
  override def join[A](caseClass: CaseClass[A, Show]): Show[A] =
    new Show[A]:
      def show(a: A) =
        val classLabel: String = caseClass.label
        val labeledParams: String = caseClass.params
          .map { param =>
            val label      = param.label          // "name"
            val show       = param.typeclass      // Show[param.PType] // Show[String]
            val value      = param.dereference(a) // param.PType       // person.name
            val shownValue = show.show(value)
            s"$label = $shownValue"
          }
          .mkString(", ")
        s"$classLabel($labeledParams)"

trait Eq[A]:
  def isSame(lhs: A, rhs: A): Boolean

object Eq extends Derivation[Eq]:
  given Eq[String] with
    def isSame(lhs: String, rhs: String): Boolean =
      lhs == rhs

  given Eq[Int] with
    def isSame(lhs: Int, rhs: Int): Boolean =
      lhs == rhs

  given Eq[Boolean] with
    def isSame(lhs: Boolean, rhs: Boolean): Boolean =
      lhs == rhs

  given Eq[Double] with
    def isSame(lhs: Double, rhs: Double): Boolean =
      lhs == rhs

  given Eq[SSN] with
    def isSame(lhs: SSN, rhs: SSN): Boolean =
      lhs == rhs

  def join[A](caseClass: CaseClass[A, Eq]): Eq[A] =
    new Eq[A]:
      def isSame(lhs: A, rhs: A): Boolean =
        caseClass.params.forall { param =>
          val a = param.dereference(lhs)
          val b = param.dereference(rhs)
          val result = param.typeclass.isSame(
            a,
            b
          )
          println(s"comparing ${param.label} ${a} == ${b} -> $result")
          result
        }

final case class User(name: String, age: Int, isAlive: Boolean, ssn: SSN, more: Double, pet: Pet) derives Show, Eq

case class Pet(name: String, numberOfSwords: Int, ssn: SSN) derives Show, Eq

@main
def mirrorExample() =
  val mirror = summon[Mirror.Of[User]]
  val pet    = Pet("Jimmy The Pet", 55, SSN("Woofwoofpetbark"))
  val pet2   = Pet("Johnny The Pet", 55, SSN("Woofwoofpetbark"))
  val user   = User("Kit", 32, false, SSN("55566688"), 55.55, pet)
  val user2  = User("Kit", 32, false, SSN("55566688"), 55.55, pet2)
  val shown  = summon[Show[User]].show(user)
  val isSame = summon[Eq[User]].isSame(user, user2)
  println(s"EQUALS? $isSame")
