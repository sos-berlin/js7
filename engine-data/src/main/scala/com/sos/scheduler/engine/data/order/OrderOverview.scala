package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import com.sos.scheduler.engine.data.job.TaskId
import java.time.Instant
import scala.language.implicitConversions
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderOverview(
  path: OrderKey,
  fileBasedState: FileBasedState,
  orderState: OrderState,
  nextStepAt: Option[Instant] = None,
  setbackUntil: Option[Instant] = None,
  taskId: Option[TaskId] = None,
  isOnBlacklist: Boolean = false,
  isSuspended: Boolean = false)
extends FileBasedOverview

object OrderOverview {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat8(apply)
}
