package js7.controller.command

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.auth.{SimpleUser, UserId}
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{Batch, CancelOrders, NoOperation, ReleaseEvents, Response}
import js7.data.order.OrderId

/**
  * @author Joacim Zschimmer
  */
final class ControllerCommandExecutorTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  private val cancelOrder = CancelOrders(Set(OrderId("ORDER-ID")), CancellationMode.FreshOnly)
  private val meta = CommandMeta(SimpleUser(UserId("USER")))

  private var cancelled = 0
  private val otherCommandExecutor = new CommandExecutor[ControllerCommand]:
    def executeCommand(command: ControllerCommand, meta: CommandMeta)
    : IO[Checked[command.Response]] =
      (command, meta) match
        case (`cancelOrder`, `meta`) =>
          cancelled += 1
          IO.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case (NoOperation(None), `meta`) =>
          IO.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case _ =>
          IO.pure(Left(Problem("COMMAND NOT IMPLEMENTED")))

  private val commandExecutor = new ControllerCommandExecutor(otherCommandExecutor)

  "NoOperation" in:
    assert(commandExecutor.executeCommand(NoOperation(), meta).await(99.s) == Right(Response.Accepted))

  "CancelOrders" in:
    assert(commandExecutor.executeCommand(cancelOrder, meta).await(99.s) == Right(Response.Accepted))
    assert(cancelled == 1)

  "Batch" in:
    assert(commandExecutor.executeCommand(Batch(
      Seq(NoOperation(), ReleaseEvents(999L), cancelOrder)
        .map(CorrelIdWrapped(CorrelId.empty, _))), meta)
        .await(99.s) ==
      Right(Batch.Response(Seq(
        Right(Response.Accepted),
        Left(Problem("COMMAND NOT IMPLEMENTED")),
        Right(Response.Accepted)))))
    assert(cancelled == 2)
