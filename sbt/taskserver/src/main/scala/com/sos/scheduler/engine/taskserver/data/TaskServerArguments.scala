package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import com.sos.scheduler.engine.common.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.common.scalautil.FileUtils.EmptyPath
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.tcp.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments.toInetSocketAddress
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import java.nio.file.Path
import java.time.Duration
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
 * @author Joacim Zschimmer
 */
final case class TaskServerArguments(
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
  def masterInetSocketAddress: InetSocketAddress = toInetSocketAddress(masterAddress)
  def logFilenamePart = {
    val jobName = startMeta.job lastIndexOf "/" match {
      case -1 ⇒ startMeta.job
      case i ⇒ startMeta.job.substring(i + 1)
    }
    s"task-${agentTaskId.string}-$jobName-${startMeta.taskId.string}"
  }
}

object TaskServerArguments {
  private val HostPortRegex = "(.*):(\\d+)".r

  def forTest(
    tcpPort: Int = 999999999,
    directory: Path = EmptyPath,
    stdFileMap: Map[StdoutStderrType, Path] = Map())
  = new TaskServerArguments(
      masterAddress = s"127.0.0.1:$tcpPort",
      startMeta = StartTask.Meta.Default,
      tunnelToken = TunnelToken(TunnelId("TEST-TUNNEL"), SecretString("TUNNEL-SECRET")),
      workingDirectory = directory,
      logDirectory = temporaryDirectory,
      stdFileMap = stdFileMap,
      agentTaskId = AgentTaskId("1-1"),
      rpcKeepaliveDurationOption = None)

  private def toInetSocketAddress(string: String): InetSocketAddress =
    string match {
      case HostPortRegex(host, port) ⇒ new InetSocketAddress(host, parseTcpPort(port))
    }

  implicit val jsonFormat: RootJsonFormat[TaskServerArguments] = jsonFormat12(apply)
}
