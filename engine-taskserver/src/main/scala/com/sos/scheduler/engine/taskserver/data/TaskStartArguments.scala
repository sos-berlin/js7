package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits._
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments.toInetSocketAddress
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import java.time.Duration
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(
  agentTaskId: AgentTaskId,
  masterAddress: String,
  tunnelToken: TunnelToken,
  environment: immutable.Iterable[(String, String)] = Nil,
  directory: Path,
  logDirectory: Path,
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  logStdoutAndStderr: Boolean = false,
  killScriptFileOption: Option[Path] = None,
  tunnelInactivityTimeoutOption: Option[Duration])
{
  def masterInetSocketAddress = toInetSocketAddress(masterAddress)
  def logFilenamePart = s"task-${agentTaskId.string}"
}

object TaskStartArguments {

  private val HostPortRegex = "(.*):(\\d+)".r

  def forTest(
    tcpPort: Int = 999999999,
    directory: Path = Paths.get(""),
    stdFileMap: Map[StdoutStderrType, Path] = Map())
  = new TaskStartArguments(
      masterAddress = s"127.0.0.1:$tcpPort",
      tunnelToken = TunnelToken(TunnelId("TEST-TUNNEL"), TunnelToken.Secret("TUNNEL-SECRET")),
      directory = directory,
      logDirectory = temporaryDirectory,
      stdFileMap = stdFileMap,
      agentTaskId = AgentTaskId("1-1"),
      tunnelInactivityTimeoutOption = None)

  private def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val MyJsonFormat = jsonFormat10(apply)
}
