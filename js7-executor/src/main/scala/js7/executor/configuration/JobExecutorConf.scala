package js7.executor.configuration

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import js7.base.system.OperatingSystem.isWindows
import js7.executor.task.TaskRunner
import monix.execution.Scheduler

final case class JobExecutorConf(
  executablesDirectory: Path,
  temporaryDirectory: Path,
  scriptInjectionAllowed: Boolean,
  newTaskRunner: TaskRunner.Factory,
  blockingJobScheduler: Scheduler)

object JobExecutorConf
{
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
}
