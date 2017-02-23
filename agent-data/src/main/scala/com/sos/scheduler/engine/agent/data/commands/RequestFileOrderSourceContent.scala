package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.FileOrderSourceContent
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Duration
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class RequestFileOrderSourceContent(
  directory: String,
  regex: String,
  duration: Duration,
  knownFiles: immutable.Set[String])
extends Command {
  type Response = FileOrderSourceContent

  override def toShortString = s"RequestFileOrderSourceContent($directory,$regex,$duration,${knownFiles.size} known files})"

  override def toStringIsLonger = knownFiles.nonEmpty
}

object RequestFileOrderSourceContent {
  val SerialTypeName = "RequestFileOrderSourceContent"
  val MaxDuration = Duration.ofDays(20) // The upper bound depends on Akka tick length (Int.MaxValue ticks, a tick can be as short as 1ms)
  implicit val MyJsonFormat = jsonFormat4(apply)
}

