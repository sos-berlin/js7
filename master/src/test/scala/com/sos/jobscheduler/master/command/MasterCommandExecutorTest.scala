package js7.master.command

import js7.base.auth.{SimpleUser, UserId}
import js7.base.problem.Problem
import js7.common.scalautil.MonixUtils.syntax._
import js7.core.command.{CommandExecutor, CommandMeta}
import js7.data.command.CancelMode
import js7.data.order.OrderId
import js7.master.data.MasterCommand
import js7.master.data.MasterCommand.{Batch, CancelOrder, ReleaseEvents, NoOperation, Response}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import scala.language.reflectiveCalls
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterCommandExecutorTest extends AnyFreeSpec
{
  private val cancelOrder = CancelOrder(OrderId("ORDER-ID"), CancelMode.NotStarted)
  private val meta = CommandMeta(SimpleUser(UserId("USER")))

  private val otherCommandExecutor = new CommandExecutor[MasterCommand] {
    var cancelled = 0

    def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Either[Problem, command.Response]] =
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

  private val commandExecutor = new MasterCommandExecutor(otherCommandExecutor)

  "NoOperation" in {
    assert(commandExecutor.executeCommand(NoOperation, meta).await(99.seconds) == Right(Response.Accepted))
  }

  "CancelOrder" in {
    assert(commandExecutor.executeCommand(cancelOrder, meta).await(99.seconds) == Right(Response.Accepted))
    assert(otherCommandExecutor.cancelled == 1)
  }

  "Batch" in {
    assert(commandExecutor.executeCommand(Batch(NoOperation :: ReleaseEvents(999) :: cancelOrder :: Nil), meta).await(99.seconds) ==
      Right(Batch.Response(Right(Response.Accepted) :: Left(Problem("COMMAND NOT IMPLEMENTED")) :: Right(Response.Accepted) :: Nil)))
    assert(otherCommandExecutor.cancelled == 2)
  }

  "detailed" in {
    assert(commandExecutor.overview.currentCommandCount == 0)
    assert(commandExecutor.overview.totalCommandCount == 6)
    assert(commandExecutor.detailed.commandRuns.isEmpty)
  }
}
