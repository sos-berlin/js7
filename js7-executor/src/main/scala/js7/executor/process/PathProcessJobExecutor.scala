package js7.executor.process

import java.nio.file.Path
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.data.job.{CommandLine, ProcessExecutable}
import js7.executor.internal.JobExecutor.warnIfNotExecutable
import js7.executor.process.PathProcessJobExecutor._
import js7.executor.process.ProcessJobExecutor.StartProcess
import js7.executor.{OrderProcess, ProcessOrder}

trait PathProcessJobExecutor extends ProcessJobExecutor
{
  protected val executable: ProcessExecutable
  protected def checkFile: Checked[Path]

  protected final def warnAboutFile(): Unit =
    checkFile match {
      case Left(problem) => logger.warn(problem.toString)
      case Right(file) => warnIfNotExecutable(file)
    }

  final def toOrderProcess(processOrder: ProcessOrder): Checked[OrderProcess] =
    checkFile
      .flatMap(file =>
        evalEnv(processOrder.evaluator, executable.env)
          .map(env =>
            makeOrderProcess(
              processOrder,
              StartProcess(
                CommandLine.fromFile(file),
                name = file.toString,
                env))))
}

object PathProcessJobExecutor
{
  private val logger = Logger[this.type]
}
