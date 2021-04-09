package js7.executor.process

import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileAttributes
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.job.{JobConf, ScriptExecutable}
import js7.executor.configuration.JobExecutorConf
import js7.executor.configuration.Problems.SignedInjectionNotAllowed
import js7.executor.process.RichProcess.tryDeleteFile
import monix.eval.Task

final class ScriptJobExecutor(
  protected val executable: ScriptExecutable,
  protected val jobConf: JobConf,
  protected val executorConf: JobExecutorConf,
  temporaryFile: Path)
extends PathProcessJobExecutor
{
  protected def checkFile = Right(temporaryFile)

  def stop = Task(tryDeleteFile(temporaryFile))
}

object ScriptJobExecutor
{
  def checked(
    executable: ScriptExecutable,
    jobConf: JobConf,
    executorConf: JobExecutorConf)
  : Checked[ScriptJobExecutor] =
    if (!executorConf.scriptInjectionAllowed)
      Left(SignedInjectionNotAllowed)
    else {
      val file = createTempFile(
        executorConf.temporaryDirectory,
        "script-",
        isWindows ?? ".cmd",
        ShellFileAttributes: _*)
      Checked
        .catchNonFatal { file.write(executable.script, JobExecutorConf.FileEncoding) }
        .flatMap(_ => Right(new ScriptJobExecutor(executable, jobConf, executorConf, file)))
    }
}
