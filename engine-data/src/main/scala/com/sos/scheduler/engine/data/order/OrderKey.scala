package com.sos.scheduler.engine.data.order

import OrderKey._
import com.fasterxml.jackson.annotation.{JsonProperty, JsonCreator}
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}
import com.sos.scheduler.engine.data.jobchain.JobChainPath

// Wegen "scala: error while loading IsString, class file OrderKey.class":  @JsonSerialize(using = classOf[OrderKeySerializer])
final case class OrderKey(jobChainPath: JobChainPath, id: OrderId)
extends SerializableOrderKey
with TypedPath {
  requireIsAbsolute()

  def fileBasedType = FileBasedType.order

  def string = jobChainPath.string + separator + id

  override def toString = s"${jobChainPath.string}:$id"
}


object OrderKey {
  private val separator = ','

  def apply(o: String): OrderKey = {
    val i = o indexOf ','
    require(i > 0, "OrderKey TypedPath needs comma ',' to separate job chain path from order key")
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
