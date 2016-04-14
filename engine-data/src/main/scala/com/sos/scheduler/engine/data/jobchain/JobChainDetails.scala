package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.generic.JavaJsonFormats
import com.sos.scheduler.engine.base.generic.JavaJsonFormats._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.filebased.{FileBasedDetails, FileBasedState}
import java.io.File
import java.time.Instant
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

final case class JobChainDetails(
  path: JobChainPath,
  fileBasedState: FileBasedState,
  file: Option[File],
  fileModificationInstant: Option[Instant],
  sourceXml: Option[String],
  nodes: immutable.Seq[NodeOverview] )
extends FileBasedDetails


object JobChainDetails {
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat6(apply)
}
