package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits._
import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(
  controllerAddress: String,
  tunnelTokenOption: Option[TunnelToken] = None,
  environment: immutable.Iterable[(String, String)] = Nil,
  directory: Path,
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  logStdoutAndStderr: Boolean = false)
{
  def controllerInetSocketAddress = toInetSocketAddress(controllerAddress)
}

object TaskStartArguments {

  private val HostPortRegex = "(.*):(\\d+)".r

  def forTest(
    tcpPort: Int = 999999999,
    directory: Path = Paths.get(""),
    stdFileMap: Map[StdoutStderrType, Path] = Map())
  = new TaskStartArguments(
      controllerAddress = s"127.0.0.1:$tcpPort",
      tunnelTokenOption = None,
      directory = directory,
      stdFileMap = stdFileMap
    )

  private[task] def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val MyJsonFormat = jsonFormat6(apply)
}
