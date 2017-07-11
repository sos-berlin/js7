package com.sos.jobscheduler.taskserver.data

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.sprayjson.SprayJson.JsonFormats._
import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType
import com.sos.jobscheduler.common.scalautil.FileUtils.EmptyPath
import com.sos.jobscheduler.common.system.FileUtils._
import com.sos.jobscheduler.common.tcp.TcpUtils.parseTcpPort
import com.sos.jobscheduler.taskserver.data.TaskServerArguments.toInetSocketAddress
import com.sos.jobscheduler.tunnel.data.{TunnelId, TunnelToken}
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
  startMeta: AgentCommand.StartTask.Meta,
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
      startMeta = AgentCommand.StartTask.Meta.Default,
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
