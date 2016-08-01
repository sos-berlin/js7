package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.queries.QueryableOrder
import java.time.Instant
import scala.collection.immutable
import scala.language.implicitConversions
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  path: OrderKey,
  fileBasedState: FileBasedState,
  sourceType: OrderSourceType,
  orderState: OrderState,
  nextStepAt: Option[Instant] = None,
  setbackUntil: Option[Instant] = None,
  taskId: Option[TaskId] = None,
  isBlacklisted: Boolean = false,
  isSuspended: Boolean = false)
extends FileBasedOverview with QueryableOrder {

  def orderKey: OrderKey = path

  def isSetback = setbackUntil.isDefined
}

object OrderOverview {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  private implicit val OrderSourceTypeJsonFormat = OrderSourceType.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat9(apply)

  implicit val ordering: Ordering[OrderOverview] = Ordering by { o ⇒ (o.orderKey.jobChainPath, o.orderState, o.orderKey.id) }

  final class Statistics(val orderOverviews: immutable.Seq[OrderOverview]) {
    def count = orderOverviews.size
    lazy val inProcessCount = orderOverviews count { _.taskId.isDefined }
    lazy val suspendedCount = orderOverviews count { _.isSuspended }
    lazy val blacklistedCount = orderOverviews count { _.isBlacklisted }
  }
}
