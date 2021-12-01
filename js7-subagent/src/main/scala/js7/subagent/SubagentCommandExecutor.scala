package js7.subagent

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.data.event.KeyedEvent.NoKey
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentCommandExecutor._
import js7.subagent.SubagentExecutor.Dedicated
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, AttachSignedItem, CoupleDirector, DedicateSubagent, KillProcess, NoOperation, ShutDown, StartOrderProcess}
import js7.subagent.data.SubagentEvent.SubagentItemAttached
import monix.eval.Task
import scala.concurrent.duration.Deadline.now

trait SubagentCommandExecutor
extends CommandExecutor
with SubagentExecutor
{
  protected def jobLauncherConf: JobLauncherConf
  protected def onStopped(termination: ProgramTermination): Task[Unit]

  protected[subagent] val dedicatedOnce = SetOnce[Dedicated]

  def executeCommand(numbered: Numbered[SubagentCommand]): Task[Checked[numbered.value.Response]] =
    Task.defer {
      val command = numbered.value
      logger.debug(s"#${numbered.number} -> $command")
      val since = now
      command
        .match_ {
          case StartOrderProcess(order, defaultArguments) =>
            startOrderProcess(order, defaultArguments)
              .rightAs(SubagentCommand.Accepted)

          case AttachItem(item) =>
            attachItem(item)
              .rightAs(SubagentCommand.Accepted)

          //case AttachUnsignedItem(item) =>
          //  journal
          //    .persistKeyedEvent(NoKey <-: SubagentItemAttached(item))
          //    .rightAs(SubagentCommand.Accepted)

          case AttachSignedItem(signed) =>
            logger.warn(s"❗️ Signature not validated for ${signed.value.key}") // FIXME Validate signature!
            journal
              .persistKeyedEvent(NoKey <-: SubagentItemAttached(signed.value))
              .rightAs(SubagentCommand.Accepted)

          case KillProcess(orderId, signal) =>
            dedicatedOnce.task
              .flatMap(_.subagentDriver
                .killProcess(orderId, signal))
              .as(Right(SubagentCommand.Accepted))

          case cmd: DedicateSubagent =>
            executeDedicateSubagent(cmd)

          case cmd: CoupleDirector =>
            executeCoupleDirector(cmd)
              .rightAs(SubagentCommand.Accepted)

          case ShutDown(processSignal, restart) =>
            logger.info(s"❗️ $command")
            shutdown(processSignal, restart)
              .as(Right(SubagentCommand.Accepted))

          case NoOperation =>
            Task.pure(Right(SubagentCommand.Accepted))
        }
        .tapEval(checked => Task(logger.debug(
          s"#${numbered.number} <- ${command.getClass.simpleScalaName}" +
            s" ${since.elapsed.pretty} => $checked")))
        .map(_.map(_.asInstanceOf[numbered.value.Response]))
    }
}

object SubagentCommandExecutor
{
  private val logger = Logger(getClass)
}
