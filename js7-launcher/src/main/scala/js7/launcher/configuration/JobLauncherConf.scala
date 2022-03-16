package js7.launcher.configuration

import java.nio.charset.Charset
import java.nio.file.Path
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
  encoding: Charset,
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
  val ErrLineLengthMaximum = 4096  // Has to fit into the journal
}
