package js7.subagent

import cats.syntax.traverse.*
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichString, *}
import js7.core.command.CommandMeta
import js7.data.event.KeyedEvent.NoKey
import js7.data.subagent.SubagentCommand
import js7.data.subagent.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, DetachProcessedOrder, KillProcess, NoOperation, ReleaseEvents, ShutDown, StartOrderProcess}
import js7.data.subagent.SubagentEvent.SubagentItemAttached
import js7.subagent.SubagentCommandExecutor.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now

private[subagent] final class SubagentCommandExecutor(
  val subagent: Subagent,
  signatureVerifier: DirectoryWatchingSignatureVerifier)
{
  private val journal = subagent.journal

  def executeCommand(numbered: Numbered[SubagentCommand], meta: CommandMeta)
  : Task[Checked[numbered.value.Response]] =
    Task.defer {
      val command = numbered.value
      logger.debug(s"#${numbered.number} -> $command")
      val since = now
      command
        .match_ {
          case StartOrderProcess(order, defaultArguments) =>
            subagent.startOrderProcess(order, defaultArguments)
              .rightAs(SubagentCommand.Accepted)

          //case AttachItem(item) =>
          //  item match {
          //    case _: SignableItem =>
          //      Task.pure(Left(Problem.pure("SignabledItem must have a signature")))
          //
          //    case item: UnsignedSimpleItem =>
          //      attachItem(item)
          //        .rightAs(SubagentCommand.Accepted)
          //  }

          case AttachSignedItem(signed) =>
            signatureVerifier.verify(signed.signedString) match {
              case Left(problem) => Task.pure(Left(problem))
              case Right(signerIds) =>
                // Duplicate with Agent
                logger.info(Logger.SignatureVerified,
                  s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")
                journal
                  .persistKeyedEvent(NoKey <-: SubagentItemAttached(signed.value))
                  .rightAs(SubagentCommand.Accepted)
            }

          case KillProcess(orderId, signal) =>
            subagent.checkedDedicatedSubagent
              .traverse(_.killProcess(orderId, signal))
              .rightAs(SubagentCommand.Accepted)

          case cmd: DedicateSubagent =>
            subagent.executeDedicateSubagent(cmd)

          case cmd: CoupleDirector =>
            Task(subagent.checkedDedicatedSubagent)
              .flatMapT(_.executeCoupleDirector(cmd))
              .rightAs(SubagentCommand.Accepted)

          case ShutDown(processSignal, dontWaitForDirector, restart) =>
            subagent.shutdown(processSignal, dontWaitForDirector, restart)
              .as(Right(SubagentCommand.Accepted))

          case DetachProcessedOrder(orderId) =>
            // DO NOT execute concurrently with a following StartOrderProcess(orderId)
            // OrderId must be released before start of next order.
            // Otherwise idempotency detection would kick in.
            subagent.detachProcessedOrder(orderId)
              .rightAs(SubagentCommand.Accepted)

          case ReleaseEvents(eventId) =>
            subagent.releaseEvents(eventId)
              .rightAs(SubagentCommand.Accepted)

          case NoOperation =>
            Task.right(SubagentCommand.Accepted)

          case SubagentCommand.Batch(correlIdWrappedCommands) =>
            // TODO Sobald Kommandos in einem Stream geschickt werden,
            //  wird Batch wohl nicht mehr gebraucht.
            // Der Stream braucht dann Kommando-Transaktionen?
            Observable
              .fromIterable(correlIdWrappedCommands)
              // mapEval is executed one by one with takeWhileInclusive
              .mapEval(_
                .bindCorrelId(subcmd =>
                  executeCommand(numbered.copy(value = subcmd), meta)
                    .map(_.map(o => o: SubagentCommand.Response))))
              .takeWhileInclusive(_.isRight) // Don't continue after first problem
              .map(_.rightAs(()))
              .foldL
              .rightAs(SubagentCommand.Accepted)
        }
        .tapEval(checked => Task(logger.debug(
          s"#${numbered.number} <- ${command.getClass.simpleScalaName}" +
            s" ${since.elapsed.pretty} => ${checked.left.map("⚠️ " + _.toString)}")))
        .map(_.map(_.asInstanceOf[numbered.value.Response]))
        .logWhenItTakesLonger(s"executeCommand(${numbered.toString.truncateWithEllipsis(100)})")
    }
}

private object SubagentCommandExecutor {
  private val logger = Logger[this.type]
}
