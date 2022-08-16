package js7.controller.command

import js7.base.auth.{SimpleUser, UserId}
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{Batch, CancelOrders, NoOperation, ReleaseEvents, Response}
import js7.data.order.OrderId
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class ControllerCommandExecutorTest extends Test
{
  private val cancelOrder = CancelOrders(Set(OrderId("ORDER-ID")), CancellationMode.FreshOnly)
  private val meta = CommandMeta(SimpleUser(UserId("USER")))

  private var cancelled = 0
  private val otherCommandExecutor = new CommandExecutor[ControllerCommand] {
    def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Either[Problem, command.Response]] =
      (command, meta) match {
        case (`cancelOrder`, `meta`) =>
          cancelled += 1
          Task.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case (NoOperation(None), `meta`) =>
          Task.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case _ =>
          Task.pure(Left(Problem("COMMAND NOT IMPLEMENTED")))
      }
  }

  private val commandExecutor = new ControllerCommandExecutor(otherCommandExecutor)

  "NoOperation" in {
    assert(commandExecutor.executeCommand(NoOperation(), meta).await(99.s) == Right(Response.Accepted))
  }

  "CancelOrders" in {
    assert(commandExecutor.executeCommand(cancelOrder, meta).await(99.s) == Right(Response.Accepted))
    assert(cancelled == 1)
  }

  "Batch" in {
    assert(commandExecutor.executeCommand(Batch(
      Seq(NoOperation(), ReleaseEvents(999L), cancelOrder)
        .map(CorrelIdWrapped(CorrelId.empty, _))), meta)
        .await(99.s) ==
      Right(Batch.Response(Seq(
        Right(Response.Accepted),
        Left(Problem("COMMAND NOT IMPLEMENTED")),
        Right(Response.Accepted)))))
    assert(cancelled == 2)
  }

  "detailed" in {
    assert(commandExecutor.overview.currentCommandCount == 0)
    assert(commandExecutor.overview.totalCommandCount == 6)
    assert(commandExecutor.detailed.commandRuns.isEmpty)
  }
}
