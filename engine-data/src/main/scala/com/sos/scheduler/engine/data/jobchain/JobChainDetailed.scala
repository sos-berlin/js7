package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import com.sos.scheduler.engine.data.filebased.{FileBasedDetailed, FileBasedState}
import java.nio.file.Path
import java.time.Instant
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

final case class JobChainDetailed(
  overview: JobChainOverview,
  file: Option[Path],
  fileModifiedAt: Option[Instant],
  sourceXml: Option[String],
  nodes: immutable.Seq[NodeOverview] )
extends FileBasedDetailed

object JobChainDetailed {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)
}
