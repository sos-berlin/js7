package js7.executor.configuration

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor
import monix.execution.Scheduler

final case class JobExecutorConf(
  executablesDirectory: Path,
  temporaryDirectory: Path,
  workingDirectory: Path,
  killScript: Option[ProcessKillScript],
  scriptInjectionAllowed: Boolean,
  implicit val iox: IOExecutor,
  blockingJobScheduler: Scheduler)

object JobExecutorConf
{
  // TODO Put this constants at a central place
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
  val ErrLineLengthMaximum = 4096  // Has to fit into the journal
}
