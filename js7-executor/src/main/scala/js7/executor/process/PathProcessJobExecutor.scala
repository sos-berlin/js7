package js7.executor.process

import java.nio.file.Path
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.executor.ProcessOrder
import js7.executor.internal.JobExecutor.warnIfNotExecutable
import js7.executor.process.PathProcessJobExecutor._
import js7.executor.process.ProcessJobExecutor.StartProcess
import monix.eval.Task

trait PathProcessJobExecutor extends ProcessJobExecutor
{
  protected val executable: ProcessExecutable
  protected def checkFile: Task[Checked[Path]]

  override final def precheckAndWarn =
    checkFile map {
      case Left(problem) => logger.warn(problem.toString)
      case Right(file) => warnIfNotExecutable(file)
    }

  final def prepareOrderProcess(processOrder: ProcessOrder) =
    checkFile
      .flatMapT(file => Task(
        evalEnv(processOrder.scope, executable.env)
          .map(env =>
            makeOrderProcess(
              processOrder,
              StartProcess(
                CommandLine.fromFile(file),
                name = file.toString,
                env)))))
}

object PathProcessJobExecutor
{
  private val logger = Logger[this.type]
}
