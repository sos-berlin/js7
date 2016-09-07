package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.filebased.FileBasedState
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

final case class JobChainDetailed(
  overview: JobChainOverview,
  nodes: immutable.Seq[NodeOverview] )

object JobChainDetailed {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat2(apply)
}
