package js7.launcher.process

import js7.base.problem.Checked
import js7.data.job.{CommandLineEvaluator, CommandLineExecutable, JobConf, JobResource, JobResourcePath}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher.warnIfNotExecutable
import js7.launcher.process.ProcessJobLauncher.StartProcess
import js7.launcher.{OrderProcess, ProcessOrder}
import monix.eval.Task

final class CommandLineJobLauncher(
  protected val executable: CommandLineExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource])
extends ProcessJobLauncher
{
  override def stop = Task.unit

  def toOrderProcess(processOrder: ProcessOrder): Task[Checked[OrderProcess]] =
    Task {
      new CommandLineEvaluator()(processOrder.scope)
        .eval(executable.commandLineExpression)
        .flatMap { commandLine =>
          warnIfNotExecutable(commandLine.file)
          evalEnv(executable.env, processOrder.scope)
            .map(env =>
              makeOrderProcess(
                processOrder,
                StartProcess(
                  commandLine,
                  name = commandLine.file.getFileName.toString,
                  env)))
        }
    }
}