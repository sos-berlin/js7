package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.{Checked, Problem}

final case class OrderId private(string: String) extends GenericString
{
  import OrderId._

  if (string.isEmpty) throw new IllegalArgumentException("OrderId must not be empty")

  override def toString = s"Order:$string"

  def pretty = s"Order $string"

  def /(childId: String): OrderId =
    this / ChildId(childId)

  def /(childId: ChildId): OrderId =
    OrderId(string + ChildSeparator + childId.string)

  def checkedNameSyntax: Checked[this.type] =
    if (string.isEmpty)
      Invalid(Problem("OrderId must not be empty"))
    else if (string.exists(ReservedCharacters))
      Invalid(Problem("OrderId must not contain reserved characters " + ReservedCharacters.mkString(", ")))
    else
      Valid(this)
    //firstProblem(string.stripPrefix("/").split('/').iterator map nameValidator.checked) match {
    //  case Some(problem) => problem withKey toString
    //  case None => Valid(this)
    //}
}

object OrderId extends GenericString.NonEmpty[OrderId]
{
  val ChildSeparator = "/"  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.
  private val ReservedCharacters = Set('/')

  protected def unchecked(string: String) = new OrderId(string)

  final case class ChildId private(string: String) extends GenericString
  object ChildId extends GenericString.NonEmpty[ChildId] {
    override val name = "OrderId.Child"

    protected def unchecked(string: String) = new ChildId(string)
  }
}
