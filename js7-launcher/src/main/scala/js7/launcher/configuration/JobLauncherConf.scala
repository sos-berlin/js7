package js7.launcher.configuration

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.common.http.configuration.RecouplingStreamReaderConf
import monix.execution.Scheduler

final case class JobLauncherConf(
  executablesDirectory: Path,
  shellScriptTmpDirectory: Path,
  tmpDirectory: Path,
  /** Working directory of the process to be started. */
  workingDirectory: Path,
  killWithSigterm: Seq[String],
  killWithSigkill: Seq[String],
  killForWindows: Seq[String],
  killScript: Option[ProcessKillScript],
  scriptInjectionAllowed: Boolean,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  implicit val iox: IOExecutor,
  blockingJobScheduler: Scheduler,
  clock: AlarmClock)

object JobLauncherConf
{
  // TODO Put this constants at a central place
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
  val ErrLineLengthMaximum = 4096  // Has to fit into the journal
}
