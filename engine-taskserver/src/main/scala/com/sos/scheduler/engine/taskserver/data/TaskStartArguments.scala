package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import com.sos.scheduler.engine.common.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.common.scalautil.FileUtils.EmptyPath
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.tcp.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments.toInetSocketAddress
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import java.nio.file.Path
import java.time.Duration
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(
  agentTaskId: AgentTaskId,
  startMeta: StartTask.Meta,
  masterAddress: String,
  tunnelToken: TunnelToken,
  environment: Map[String, String] = Map(),
  workingDirectory: Path,
  logDirectory: Path,
  dotnet: DotnetConfiguration = DotnetConfiguration(),
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  logStdoutAndStderr: Boolean = false,
  killScriptOption: Option[ProcessKillScript] = None,
  rpcKeepaliveDurationOption: Option[Duration])
{
  def masterInetSocketAddress = toInetSocketAddress(masterAddress)
  def logFilenamePart = s"task-${agentTaskId.string}-${startMeta.job.name}-${startMeta.taskId.string}"
}

object TaskStartArguments {
  private val HostPortRegex = "(.*):(\\d+)".r

  def forTest(
    tcpPort: Int = 999999999,
    directory: Path = EmptyPath,
    stdFileMap: Map[StdoutStderrType, Path] = Map())
  = new TaskStartArguments(
      masterAddress = s"127.0.0.1:$tcpPort",
      startMeta = StartTask.Meta.Default,
      tunnelToken = TunnelToken(TunnelId("TEST-TUNNEL"), TunnelToken.Secret("TUNNEL-SECRET")),
      workingDirectory = directory,
      logDirectory = temporaryDirectory,
      stdFileMap = stdFileMap,
      agentTaskId = AgentTaskId("1-1"),
      rpcKeepaliveDurationOption = None)

  private def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val MyJsonFormat = jsonFormat12(apply)
}
