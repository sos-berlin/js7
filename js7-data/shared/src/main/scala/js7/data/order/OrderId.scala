package js7.data.order

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import scala.util.matching.Regex

final case class OrderId private(string: String) extends GenericString:
  import OrderId.*

  if string.isEmpty then throw new IllegalArgumentException("OrderId must not be empty")

  // Faster than generic hashCode of the case class:
  override def hashCode: Int =
    string.hashCode

  override def toString =
    s"Order:$string"

  def pretty: String =
    s"Order $string"

  def /(childId: String): OrderId =
    withChild(childId).orThrow

  def withChild(childId: String): Checked[OrderId] =
    if childId.isEmpty then
      Left(EmptyStringProblem("OrderId.withChild"))
    else if childId.exists(ReservedCharacters) then
      Left(Problem("OrderId must not contain reserved characters: " +
        ReservedCharacters.mkString(", ")))
    else
      Right(new OrderId(string + ChildSeparator + childId))

  def allParents: Seq[OrderId] =
    string.split(ChildSeparatorRegex)
      .view
      .dropRight(1)
      .scanLeft(Vector.empty[String])(_ :+ _)
      .drop(1) /*neutral element, the empty string*/
      .map(_.mkString(ChildSeparator))
      .map(OrderId.apply)
      .toVector

  def root: OrderId =
    string indexOf ChildSeparator match
      case -1 => this
      case n => OrderId(string take n)

  def isRoot: Boolean =
    string.indexOf(ChildSeparator) == -1

  def checkedNameSyntax: Checked[this.type] =
    if string.isEmpty then
      Left(EmptyStringProblem("OrderId"))
    else if string.exists(ReservedCharacters) then
      Left(Problem("OrderId must not contain reserved characters: " +
        ReservedCharacters.mkString(", ")))
    else
      Right(this)


object OrderId extends GenericString.NonEmpty[OrderId]:
  val ChildSeparatorChar = '|'
  val ChildSeparator: String = ChildSeparatorChar.toString
  private val ChildSeparatorRegex = Regex.quote(ChildSeparator)
  private val ReservedCharacters = Set('|')

  protected def unchecked(string: String) = new OrderId(string)

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validOrderId: String): OrderId =
    apply(validOrderId)

  final case class ChildId private(string: String) extends GenericString
  object ChildId extends GenericString.NonEmpty[ChildId]:
    override val name = "OrderId.Child"

    protected def unchecked(string: String) =
      new ChildId(string)

    override def checked(string: String): Checked[ChildId] =
      if string contains ChildSeparator then
        Left(Problem.pure(s"Order ChildId must not contain '$ChildSeparator'"))
      else
        super.checked(string)
