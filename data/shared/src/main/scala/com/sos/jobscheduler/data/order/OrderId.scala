package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.generic.IsString

final case class OrderId(string: String) extends IsString {

  override def toString = s"Order:$string"

  // TODO Sicherstellen, dass Schrägstrich in einer OrderId nur hier verwendet wird, damit sie eindeutig ist.
  def child(childString: String): OrderId =
    OrderId(s"$string/$childString")
}

object OrderId extends IsString.Companion[OrderId]
