package js7.launcher.process

import cats.effect.IO
import java.nio.file.Path
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.launcher.ProcessOrder
import js7.launcher.internal.JobLauncher.warnIfNotExecutable
import js7.launcher.process.PathProcessJobLauncher.*
import js7.launcher.process.ProcessJobLauncher.StartProcess

trait PathProcessJobLauncher extends ProcessJobLauncher:
  protected val executable: ProcessExecutable
  protected def checkFile: IO[Checked[Path]]

  override final def precheckAndWarn =
    checkFile map:
      case Left(problem) => logger.warn(problem.toString)
      case Right(file) => warnIfNotExecutable(file)

  final def toOrderProcess(processOrder: ProcessOrder) =
    checkFile
      .flatMapT(file => IO(
        ProcessOrder.evalEnv(executable.env, processOrder.scope)
          .map(env =>
            makeOrderProcess(
              processOrder,
              StartProcess(
                CommandLine.fromFile(file),
                name = file.toString,
                env)))))


object PathProcessJobLauncher:
  private val logger = Logger[this.type]
