package js7.subagent

import cats.syntax.traverse.*
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.subagent.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, DetachProcessedOrder, KillProcess, NoOperation, ReleaseEvents, ShutDown, StartOrderProcess}
import js7.data.subagent.SubagentEvent.SubagentItemAttached
import js7.data.subagent.{SubagentCommand, SubagentState}
import js7.journal.InMemoryJournal
import js7.subagent.SubagentCommandExecuter.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now

private final class SubagentCommandExecuter(
  val subagent: Subagent,
  signatureVerifier: DirectoryWatchingSignatureVerifier,
  journal: InMemoryJournal[SubagentState])
{
  def executeCommand(numbered: Numbered[SubagentCommand]): Task[Checked[numbered.value.Response]] =
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
            subagent.checkedDedicated
              .traverse(_
                .localSubagentDriver
                .killProcess(orderId, signal))
              .rightAs(SubagentCommand.Accepted)

          case cmd: DedicateSubagent =>
            subagent.executeDedicateSubagent(cmd)

          case cmd: CoupleDirector =>
            subagent.executeCoupleDirector(cmd)
              .rightAs(SubagentCommand.Accepted)

          case ShutDown(processSignal, dontWaitForDirector, restart) =>
            logger.info(s"❗ $command")
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
            Task.pure(Right(SubagentCommand.Accepted))

          case SubagentCommand.Batch(correlIdWrappedCommands) =>
            // TODO Sobald Kommandos in einem Stream geschickt werden,
            //  wird Batch wohl nicht mehr gebraucht.
            // Der Stream braucht dann Kommando-Transaktionen?
            Observable
              .fromIterable(correlIdWrappedCommands)
              // mapEval is executed one by one with takeWhileInclusive
              .mapEval(_
                .bindCorrelId(subcmd =>
                  executeCommand(numbered.copy(value = subcmd))
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
    }
}

private object SubagentCommandExecuter {
  private val logger = Logger(getClass)
}