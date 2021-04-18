package js7.executor.process

import js7.base.problem.Checked
import js7.data.job.{CommandLineEvaluator, CommandLineExecutable, JobConf, JobResource, JobResourceId}
import js7.executor.configuration.JobExecutorConf
import js7.executor.internal.JobExecutor.warnIfNotExecutable
import js7.executor.process.ProcessJobExecutor.StartProcess
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task

final class CommandLineJobExecutor(
  protected val executable: CommandLineExecutable,
  protected val jobConf: JobConf,
  protected val jobExecutorConf: JobExecutorConf,
  protected val idToJobResource: JobResourceId => Checked[JobResource])
extends ProcessJobExecutor
{
  override def stop = Task.unit

  def toOrderProcess(processOrder: ProcessOrder): Checked[OrderProcess] =
    new CommandLineEvaluator(processOrder.evaluator)
      .eval(executable.commandLineExpression)
      .flatMap { commandLine =>
        warnIfNotExecutable(commandLine.file)
        evalEnv(processOrder.evaluator, executable.env)
          .flatMap(env =>
            Right(makeOrderProcess(
              processOrder,
              StartProcess(
                commandLine,
                name = commandLine.file.getFileName.toString,
                env))))
      }
}
