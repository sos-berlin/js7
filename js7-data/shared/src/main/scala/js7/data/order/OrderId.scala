package js7.data.order

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}

final case class OrderId private(string: String) extends GenericString
{
  import OrderId._

  if (string.isEmpty) throw new IllegalArgumentException("OrderId must not be empty")

  override def toString = s"Order:$string"

  def pretty = s"Order $string"

  def |(childId: String): OrderId =
    this | ChildId(childId)

  def |(childId: ChildId): OrderId =
    OrderId(string + ChildSeparator + childId.string)

  def checkedNameSyntax: Checked[this.type] =
    if (string.isEmpty)
      Left(Problem("OrderId must not be empty"))
    else if (string.exists(ReservedCharacters))
      Left(Problem("OrderId must not contain reserved characters " + ReservedCharacters.mkString(", ")))
    else
      Right(this)
    //firstProblem(string.stripPrefix("/").split('/').iterator map nameValidator.checked) match {
    //  case Some(problem) => problem withKey toString
    //  case None => Right(this)
    //}
}

object OrderId extends GenericString.NonEmpty[OrderId]
{
  val ChildSeparator = "|"
  private val ReservedCharacters = Set('|')

  protected def unchecked(string: String) = new OrderId(string)

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validOrderId: String) =
    apply(validOrderId)

  final case class ChildId private(string: String) extends GenericString
  object ChildId extends GenericString.NonEmpty[ChildId] {
    override val name = "OrderId.Child"

    protected def unchecked(string: String) = new ChildId(string)
  }
}
