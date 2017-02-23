package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.generic.IsString

final case class OrderId(string: String) extends IsString {
  override def toString = s"Order:$string"
}

object OrderId extends IsString.Companion[OrderId]
