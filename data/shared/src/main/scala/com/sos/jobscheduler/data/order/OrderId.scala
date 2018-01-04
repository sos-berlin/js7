package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.generic.IsString

final case class OrderId(string: String) extends IsString {
  require(string.nonEmpty, "OrderId must not be empty")

  override def toString = s"Order:$string"

  def pretty = s"Order $string"

  def /(child: OrderId.Child): OrderId =
    OrderId(string + OrderId.ChildSeparator + child.string)
}

object OrderId extends IsString.Companion[OrderId] {
  val ChildSeparator = "/"  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.

  final case class Child(string: String) extends IsString {
    if (string.isEmpty) throw new IllegalArgumentException("OrderId.Child must not be empty")
  }
  object Child extends IsString.Companion[Child]
}
