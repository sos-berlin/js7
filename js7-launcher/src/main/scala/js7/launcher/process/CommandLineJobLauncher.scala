package js7.launcher.process

import cats.effect.IO
import js7.base.problem.Checked
import js7.data.job.{CommandLineEvaluator, CommandLineExecutable, JobConf}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher.warnIfNotExecutable
import js7.launcher.process.ProcessJobLauncher.StartProcess
import js7.launcher.{OrderProcess, ProcessOrder}

final class CommandLineJobLauncher(
  protected val executable: CommandLineExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends ProcessJobLauncher:

  override def stop: IO[Unit] = 
    IO.unit

  def toOrderProcess(processOrder: ProcessOrder): IO[Checked[OrderProcess]] =
    IO:
      new CommandLineEvaluator()(processOrder.scope)
        .eval(executable.commandLineExpression)
        .flatMap { commandLine =>
          warnIfNotExecutable(commandLine.file)
          ProcessOrder.evalEnv(executable.env, processOrder.scope)
            .map(env =>
              makeOrderProcess(
                processOrder,
                StartProcess(
                  commandLine,
                  name = commandLine.file.getFileName.toString,
                  env)))
        }
