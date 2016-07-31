package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JobChainOverview(
  path: JobChainPath,
  fileBasedState: FileBasedState,
  isDistributed: Boolean
) extends FileBasedOverview with QueryableJobChain

object JobChainOverview {
  private implicit val fileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat3(apply)
}
