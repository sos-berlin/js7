package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments._
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json.{JsString, JsValue, JsonFormat}

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

  implicit object PathJsonFormat extends JsonFormat[Path] {
    def write(o: Path) = JsString(o.toString)
    def read(o: JsValue) = Paths.get(cast[JsString](o).value)
  }

  implicit val MyJsonFormat = jsonFormat5(apply)
}
