package js7.agent.subagent

import js7.agent.subagent.CommandDispatcherTest._
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class CommandDispatcherTest extends AnyFreeSpec
{
  "test" in {
    val dispatcher = new CommandDispatcher {
      val name = "DISPATCHER"
      type Command = CommandDispatcherTest.Command
      type Response = CommandDispatcherTest.Response
      protected val postCommand =
        _.value match {
          case Command("A") => Task.pure(Right(Response("a")))
          case _ => Task.pure(Left(Problem("FAILED")))
        }
    }
    dispatcher.start.await(99.s)

    assert(dispatcher.executeCommand(Command("A")).await(99.s) == Right(Response("a")))
    assert(dispatcher.executeCommand(Command("B")).await(99.s) == Left(Problem("FAILED")))
  }

  "Repeat failed command after delay" in {
    pending
  }
}

object CommandDispatcherTest {
  private final case class Command(string: String)
  private final case class Response(string: String)
}
