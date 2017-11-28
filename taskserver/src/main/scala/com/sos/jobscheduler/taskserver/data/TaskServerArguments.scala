package com.sos.jobscheduler.taskserver.data

import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.{DurationDecoder, DurationEncoder, PathJsonCodec}
import com.sos.jobscheduler.common.scalautil.FileUtils.EmptyPath
import com.sos.jobscheduler.common.system.FileUtils._
import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType
import com.sos.jobscheduler.data.workflow.JobPath
import io.circe.generic.JsonCodec
import java.nio.file.Path
import java.time.Duration

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class TaskServerArguments(
  agentTaskId: AgentTaskId,
  jobPath: JobPath,
  environment: Map[String, String] = Map(),
  workingDirectory: Path,
  logDirectory: Path,
  dotnet: DotnetConfiguration = DotnetConfiguration(),
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  logStdoutAndStderr: Boolean = false,
  killScriptOption: Option[ProcessKillScript] = None,
  rpcKeepaliveDurationOption: Option[Duration])
{
  def logFilenamePart =
    s"task-${jobPath.withoutStartingSlash}-${agentTaskId.string}"
}

object TaskServerArguments {
  (DurationEncoder, DurationDecoder, PathJsonCodec)  // For IntelliJ import

  def forTest(
    tcpPort: Int = 999999999,
    directory: Path = EmptyPath,
    stdFileMap: Map[StdoutStderrType, Path] = Map())
  = new TaskServerArguments(
      agentTaskId = AgentTaskId("1-1"),
      JobPath("/TEST-JOB-PATH"),
      workingDirectory = directory,
      logDirectory = temporaryDirectory,
      stdFileMap = stdFileMap,
      rpcKeepaliveDurationOption = None)
}
