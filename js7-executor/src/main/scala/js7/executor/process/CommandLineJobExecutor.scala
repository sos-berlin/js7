package js7.executor.process

import js7.base.problem.Checked
import js7.data.job.{CommandLineEvaluator, CommandLineExecutable, JobConf}
import js7.data.value.expression.Evaluator
import js7.executor.configuration.JobExecutorConf
import js7.executor.internal.JobExecutor.warnIfNotExecutable
import js7.executor.process.ProcessJobExecutor.StartProcess
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task

final class CommandLineJobExecutor(
  protected val executable: CommandLineExecutable,
  protected val jobConf: JobConf,
  protected val executorConf: JobExecutorConf)
extends ProcessJobExecutor
{
  override def stop = Task.unit

  def processOrder(processOrder: ProcessOrder): Checked[OrderProcess] = {
    val evaluator = Evaluator(toScope(processOrder))
    new CommandLineEvaluator(evaluator)
      .eval(executable.commandLineExpression)
      .flatMap { commandLine =>
        warnIfNotExecutable(commandLine.file)
        evalEnv(evaluator, executable.env)
          .flatMap(env =>
            Right(toOrderProcess(
              processOrder,
              StartProcess(commandLine, name = commandLine.file.getFileName.toString, env))))
      }
    }
}
