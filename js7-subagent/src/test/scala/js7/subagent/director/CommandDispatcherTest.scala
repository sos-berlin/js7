package js7.subagent.director

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Base64UUID
import js7.data.command.CommonCommand
import js7.data.subagent.SubagentRunId
import js7.subagent.director.CommandDispatcherTest.*

final class CommandDispatcherTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "test" in:
    val dispatcher = new CommandDispatcher:
      type Command = CommandDispatcherTest.Command
      type Response = CommandDispatcherTest.Response

      protected val name = "DISPATCHER"

      protected val postCommand = (numberedCommand, subagentRunId, isStopped) =>
        assert(subagentRunId == CommandDispatcherTest.subagentRunId)
        isStopped.isOn.map(assert(_)) *>
          numberedCommand.value.match
            case Command("A") => IO.pure(Right(Response("a")))
            case _ => IO.pure(Left(Problem("FAILED")))
    dispatcher.start(subagentRunId).await(99.s)
    assert(dispatcher.executeCommand(Command("A")).await(99.s) == Right(Response("a")))
    assert(dispatcher.executeCommand(Command("B")).await(99.s) == Left(Problem("FAILED")))

  "Repeat failed command after delay" in:
    pending


object CommandDispatcherTest:
  private val subagentRunId = SubagentRunId(Base64UUID.ffff)

  private final case class Command(string: String) extends CommonCommand
  private final case class Response(string: String)
