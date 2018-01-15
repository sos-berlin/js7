package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.generic.IsString

final case class OrderId(string: String) extends IsString {
  require(string.nonEmpty, "OrderId must not be empty")

  override def toString = s"Order:$string"

  def pretty = s"Order $string"

  def /(child: OrderId.ChildId): OrderId =
    OrderId(string + OrderId.ChildSeparator + child.string)
}

object OrderId extends IsString.Companion[OrderId] {
  val ChildSeparator = "/"  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.

  final case class ChildId(string: String) extends IsString {
    if (string.isEmpty) throw new IllegalArgumentException("OrderId.ChildId must not be empty")
  }
  object ChildId extends IsString.Companion[ChildId]
}
