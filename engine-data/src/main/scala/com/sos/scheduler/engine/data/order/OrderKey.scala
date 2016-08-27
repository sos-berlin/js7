package com.sos.scheduler.engine.data.order

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}
import com.sos.scheduler.engine.data.jobchain.JobChainPath

// Wegen "scala: error while loading IsString, class file OrderKey.class":  @JsonSerialize(using = classOf[OrderKeySerializer])
final case class OrderKey(jobChainPath: JobChainPath, id: OrderId)
extends SerializableOrderKey
with TypedPath {
  import com.sos.scheduler.engine.data.order.OrderKey._

  validate()

  def companion = OrderKey

  lazy val string = jobChainPath.string + Separator + id.string

  override lazy val name = jobChainPath.name + Separator + id.string
}

object OrderKey extends TypedPath.Companion[OrderKey] {

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.Order

  private val Separator = ','

  override implicit val ordering: Ordering[OrderKey] = Ordering by { o ⇒ (o.jobChainPath, o.id) }

  def apply(o: String): OrderKey = {
    val i = o indexOf ','
    require(i > 0, s"OrderKey TypedPath needs comma ',' to separate JobChainPath from OrderId: OrderKey($o)")
    apply(o.substring(0, i), o.substring(i + 1))
  }

  @JsonCreator
  def apply(
      @JsonProperty("jobChainPath") jobChainPath: String,
      @JsonProperty("id") id: String): OrderKey =
    OrderKey(JobChainPath(jobChainPath), OrderId(id))

  def of(jobChainPath: String, id: String): OrderKey =
    OrderKey(JobChainPath(jobChainPath), OrderId(id))
}
