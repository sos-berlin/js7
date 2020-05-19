package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import java.time.Instant
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

@javaApi
final case class JFreshOrder(underlying: FreshOrder)
extends JJsonable[JFreshOrder]
{
  protected type Underlying = FreshOrder

  protected def companion = JFreshOrder
}

@javaApi
object JFreshOrder extends JJsonable.Companion[JFreshOrder]
{
  def of(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledFor: java.util.Optional[Instant],
    arguments: java.util.Map[String, String] = java.util.Collections.emptyMap())
  : JFreshOrder =
  JFreshOrder(FreshOrder(id, workflowPath,
    scheduledFor.toScala.map(o => Timestamp.ofEpochMilli(o.toEpochMilli)),
    arguments.asScala.toMap))

  override def fromJson(jsonString: String): VEither[Problem, JFreshOrder] =
    super.fromJson(jsonString)

  def jsonEncoder = implicitly[Encoder[FreshOrder]]

  def jsonDecoder = implicitly[Decoder[FreshOrder]]
}
