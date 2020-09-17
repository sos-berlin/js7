package js7.controller.command

import js7.base.auth.{SimpleUser, UserId}
import js7.base.problem.Problem
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{Batch, CancelOrders, NoOperation, ReleaseEvents, Response}
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.data.command.CancelMode
import js7.data.order.OrderId
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class ControllerCommandExecutorTest extends AnyFreeSpec
{
  private val cancelOrder = CancelOrders(Set(OrderId("ORDER-ID")), CancelMode.NotStarted)
  private val meta = CommandMeta(SimpleUser(UserId("USER")))

  private val otherCommandExecutor = new CommandExecutor[ControllerCommand] {
    var cancelled = 0

    def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Either[Problem, command.Response]] =
      (command, meta) match {
        case (`cancelOrder`, `meta`) =>
          cancelled += 1
          Task.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case (NoOperation, `meta`) =>
          Task.pure(Right(Response.Accepted.asInstanceOf[command.Response]))
        case _ =>
          Task.pure(Left(Problem("COMMAND NOT IMPLEMENTED")))
      }
  }

  private val commandExecutor = new ControllerCommandExecutor(otherCommandExecutor)

  "NoOperation" in {
    assert(commandExecutor.executeCommand(NoOperation, meta).await(99.seconds) == Right(Response.Accepted))
  }

  "CancelOrders" in {
    assert(commandExecutor.executeCommand(cancelOrder, meta).await(99.seconds) == Right(Response.Accepted))
    assert(otherCommandExecutor.cancelled == 1)
  }

  "Batch" in {
    assert(commandExecutor.executeCommand(Batch(NoOperation :: ReleaseEvents(999L) :: cancelOrder :: Nil), meta).await(99.seconds) ==
      Right(Batch.Response(Right(Response.Accepted) :: Left(Problem("COMMAND NOT IMPLEMENTED")) :: Right(Response.Accepted) :: Nil)))
    assert(otherCommandExecutor.cancelled == 2)
  }

  "detailed" in {
    assert(commandExecutor.overview.currentCommandCount == 0)
    assert(commandExecutor.overview.totalCommandCount == 6)
    assert(commandExecutor.detailed.commandRuns.isEmpty)
  }
}
