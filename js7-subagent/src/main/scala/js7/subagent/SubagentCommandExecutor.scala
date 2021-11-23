package js7.subagent

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.stream.Numbered
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Base64UUID, ProgramTermination, SetOnce}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.subagent.SubagentRunId
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentCommandExecutor._
import js7.subagent.SubagentExecutor.Dedicated
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, AttachSignedItem, DedicateSubagent, KillProcess, NoOperation, ShutDown, StartOrderProcess}
import js7.subagent.data.SubagentEvent.SubagentItemAttached
import monix.eval.Task
import scala.concurrent.duration.Deadline.now

trait SubagentCommandExecutor
extends CommandExecutor
with SubagentExecutor
{
  protected def jobLauncherConf: JobLauncherConf
  protected def onStopped(termination: ProgramTermination): Task[Unit]

  private val subagentRunId = SubagentRunId(Base64UUID.random())
  protected[subagent] val dedicated = SetOnce[Dedicated]

  def executeCommand(numbered: Numbered[SubagentCommand]): Task[Checked[numbered.value.Response]] =
    Task.defer {
      val command = numbered.value
      logger.debug(s"#${numbered.number} -> $command")
      val since = now
      command
        .match_ {
          case DedicateSubagent(subagentId, agentPath, controllerId) =>
            // TODO When recoupling, check agentPath and subagentId!
            Task {
              val d = new Dedicated(
                newLocalSubagentDriver(subagentId, controllerId))
              if (!dedicated.trySet(d))
                Left(Problem.pure(
                  s"This Subagent has already been dedicated to ${this.dedicated}"))
              else
                Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst))
            }

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

          case StartOrderProcess(order, defaultArguments) =>
            startOrderProcess(order, defaultArguments)
              .rightAs(SubagentCommand.Accepted)

          case KillProcess(orderId, signal) =>
            dedicated.task
              .flatMap(_.subagentDriver
                .killProcess(orderId, signal))
              .as(Right(SubagentCommand.Accepted))

          case ShutDown(processSignal, restart) =>
            logger.info(s"❗️ $command")
            stop(processSignal, restart)
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
