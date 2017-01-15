package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.filebased.TypedPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath

final case class OrderKey(jobChainPath: JobChainPath, id: OrderId)
extends TypedPath {
  import com.sos.scheduler.engine.data.order.OrderKey._

  validate()

  def companion = OrderKey

  lazy val string = jobChainPath.string + Separator + id.string

  override lazy val name = jobChainPath.name + Separator + id.string
}

object OrderKey extends TypedPath.Companion[OrderKey] {

  private val Separator = ','

  override val camelName = "Order"
  override implicit val ordering: Ordering[OrderKey] = Ordering by { o ⇒ (o.jobChainPath, o.id) }

  def apply(o: String): OrderKey = {
    val i = o indexOf ','
    require(i > 0, s"OrderKey TypedPath needs comma ',' to separate JobChainPath from OrderId: OrderKey($o)")
    apply(o.substring(0, i), o.substring(i + 1))
  }

  def apply(jobChainPath: String, id: String): OrderKey =
    OrderKey(JobChainPath(jobChainPath), OrderId(id))

  def of(jobChainPath: String, id: String): OrderKey =
    OrderKey(JobChainPath(jobChainPath), OrderId(id))

  override protected[engine] def isCommaAllowed = true
}
