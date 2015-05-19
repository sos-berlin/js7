package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits.PathJsonFormat
import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments._
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(
  controllerAddress: String,
  environment: immutable.Iterable[(String, String)] = Nil,
  stdoutFile: Path = Paths.get(""),
  stderrFile: Path = Paths.get(""),
  logStdoutAndStderr: Boolean = false)
{
  def controllerInetSocketAddress = toInetSocketAddress(controllerAddress)
}

object TaskStartArguments {

  private val HostPortRegex = "(.*):(\\d+)".r

  private[task] def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val MyJsonFormat = jsonFormat5(apply)
}
