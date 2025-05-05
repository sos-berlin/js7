package js7.subagent

import cats.effect.IO
import cats.syntax.traverse.*
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.{RichString, *}
import js7.core.command.CommandMeta
import js7.data.event.KeyedEvent.NoKey
import js7.data.subagent.SubagentCommand
import js7.data.subagent.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, DetachProcessedOrder, KillProcess, NoOperation, ReleaseEvents, ShutDown, StartOrderProcess}
import js7.data.subagent.SubagentEvent.SubagentItemAttached
import js7.subagent.SubagentCommandExecutor.*
import scala.concurrent.duration.Deadline.now

private[subagent] final class SubagentCommandExecutor(
  val subagent: Subagent,
  signatureVerifier: DirectoryWatchingSignatureVerifier):

  private val journal = subagent.journal

  def executeCommand(numbered: Numbered[SubagentCommand], meta: CommandMeta)
  : IO[Checked[numbered.value.Response]] =
    IO.defer:
      val command = numbered.value
      logger.debug(s"#${numbered.number} -> $command")
      val since = now
      command
        .match
          case StartOrderProcess(order, executeDefaultArguments) =>
            subagent.startOrderProcess(order, executeDefaultArguments)
              .rightAs(SubagentCommand.Accepted)

          //case AttachItem(item) =>
          //  item match {
          //    case _: SignableItem =>
          //      IO.pure(Left(Problem.pure("SignabledItem must have a signature")))
          //
          //    case item: UnsignedSimpleItem =>
          //      attachItem(item)
          //        .rightAs(SubagentCommand.Accepted)
          //  }

          case AttachSignedItem(signed) =>
            // Duplicate with Agent
            journal.aggregate.flatMap: state =>
              if state.keyToItem.get(signed.value.key).contains(signed.value) then
                IO.right(SubagentCommand.Accepted)
              else
                signatureVerifier.verify(signed.signedString) match
                  case Left(problem) =>
                    logger.warn(s"${signed.value.key} could not be verified: $problem")
                    IO.pure(Left(problem))
                  case Right(signerIds) =>
                    logger.info(Logger.SignatureVerified,
                      s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")
                    journal
                      .persistKeyedEvent(NoKey <-: SubagentItemAttached(signed.value))
                      .rightAs(SubagentCommand.Accepted)

          case KillProcess(orderId, signal) =>
            subagent.checkedDedicatedSubagent
              .traverse(_.killProcess(orderId, signal))
              .rightAs(SubagentCommand.Accepted)

          case cmd: DedicateSubagent =>
            subagent.executeDedicateSubagent(cmd)

          case cmd: CoupleDirector =>
            IO(subagent.checkedDedicatedSubagent)
              .flatMapT(_.executeCoupleDirector(cmd))
              .rightAs(SubagentCommand.Accepted)

          case ShutDown(processSignal, dontWaitForDirector, restart) =>
            subagent.shutdown(processSignal, dontWaitForDirector = dontWaitForDirector, restart = restart)
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
            IO.right(SubagentCommand.Accepted)

          case SubagentCommand.Batch(correlIdWrappedCommands) =>
            // TODO Sobald Kommandos in einem Stream geschickt werden,
            //  wird Batch wohl nicht mehr gebraucht.
            // Der Stream braucht dann Kommando-Transaktionen?
            Stream
              .iterable(correlIdWrappedCommands)
              // mapEval is executed one by one with takeWhileInclusive
              .evalMap(_
                .bindCorrelId(subcmd =>
                  executeCommand(numbered.copy(value = subcmd), meta)
                    .map(_.map(o => o: SubagentCommand.Response))))
              .takeThrough(_.isRight) // Don't continue after first problem
              .map(_.rightAs(()))
              .compile.foldMonoid
              .rightAs(SubagentCommand.Accepted)
        .flatTap(checked => IO(logger.debug(
          s"#${numbered.number} <- ${command.getClass.simpleScalaName}" +
            s" ${since.elapsed.pretty} => ${checked.left.map("⚠️ " + _.toString)}")))
        .map(_.map(_.asInstanceOf[numbered.value.Response]))
        .logWhenItTakesLonger(s"executeCommand(${numbered.toString.truncateWithEllipsis(100)})")

private object SubagentCommandExecutor:
  private val logger = Logger[this.type]
