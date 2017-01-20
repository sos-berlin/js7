package com.sos.scheduler.engine.agent.data.commandresponses

import java.time.Instant
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class FileOrderSourceContent(files: immutable.Seq[FileOrderSourceContent.Entry])
extends Response {
  def isEmpty = files.isEmpty
}

object FileOrderSourceContent {
  final case class Entry(path: String, lastModifiedTime: Long) {
    override def toString = s"Entry($path,${Instant.ofEpochMilli(lastModifiedTime)})"
  }

  object Entry {
    implicit val MyJsonFormat = jsonFormat2(apply)
  }

  implicit val MyJsonFormat = jsonFormat1(apply)
}
