package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.filebased.NameValidator

final case class OrderId(string: String) extends GenericString
{
  import OrderId._

  if (string.isEmpty) throw new IllegalArgumentException("OrderId must not be empty")

  override def toString = s"Order:$string"

  def pretty = s"Order $string"

  def /(childId: String): OrderId =
    this / (ChildId(childId))

  def /(childId: ChildId): OrderId =
    OrderId(string + ChildSeparator + childId.string)

  def checkedNameSyntax: Checked[this.type] =
    NameValidator.checked(string) map (_ ⇒ this) match {
      case Invalid(problem) ⇒ Invalid(problem withKey "OrderId")
      case Valid(_) ⇒ Valid(this)
    }
    //firstProblem(string.stripPrefix("/").split('/').iterator map nameValidator.checked) match {
    //  case Some(problem) ⇒ problem withKey toString
    //  case None ⇒ Valid(this)
    //}
}

object OrderId extends GenericString.Companion[OrderId] {
  val ChildSeparator = "/"  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.

  final case class ChildId(string: String) extends GenericString {
    if (string.isEmpty) throw new IllegalArgumentException("OrderId.ChildId must not be empty")
  }
  object ChildId extends GenericString.Companion[ChildId]
}
