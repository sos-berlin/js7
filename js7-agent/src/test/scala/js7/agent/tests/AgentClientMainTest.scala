package js7.agent.tests

import cats.effect.ExitCode
import cats.effect.unsafe.IORuntime
import js7.agent.client.main.AgentClientMain
import js7.agent.client.main.AgentClientMain.Conf
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString, RichJson}
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.test.OurAsyncTestSuite
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class AgentClientMainTest extends OurAsyncTestSuite, BeforeAndAfterAll, TestAgentProvider:

  private given IORuntime = ioRuntime

  override def afterAll() = closer.closeThen(super.afterAll())

  //override protected def agentTestWiring = RunningAgent.TestWiring(
  //  commandHandler = Some(new CommandHandler {
  //    def execute(command: AgentCommand, meta: CommandMeta): IO[Checked[command.Response]] =
  //      IO {
  //        (command match {
  //          case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
  //          case _ => fail()
  //        }).map(_.asInstanceOf[command.Response])
  //      }
  //  }))

  "main" in:
    val output = mutable.Buffer.empty[String]
    val commandJson = json"""{ "TYPE": "ShutDown", "processSignal": "SIGTERM" }"""
    AgentClientMain
      .program(
        Conf.args(
          s"--data-directory=$dataDirectory",
          agent.localUri.toString,
          commandJson.compactPrint,
          "?"),
        output += _)
      .map: _ =>
        assert(output.size == 2)
        assert(output(0).parseJson == Right(json"""{ "TYPE": "Accepted" }"""))
        output(1).parseJsonOrThrow.fieldOrThrow("startedAt")
        succeed

  "main with Agent URI only checks whether Agent is responding (it is)" in:
    val output = mutable.Buffer.empty[String]
    AgentClientMain
      .program(
        Conf.args(
          s"--data-directory=$dataDirectory",
          agent.localUri.toString),
        o => output += o)
      .map: exitCode =>
        assert(exitCode == ExitCode.Success && output == List("JS7 Agent is responding"))

  "main with Agent URI only checks whether Agent is responding (it is not)" in:
    val port = findFreeTcpPort()
    val output = mutable.Buffer.empty[String]
    AgentClientMain
      .program(
        Conf.args(
          s"--data-directory=$dataDirectory",
          s"http://127.0.0.1:$port"),
        output += _)
      .map: exitCode =>
        assert(exitCode == ExitCode.Error)
        assert(output.head.contains("JS7 Agent is not responding: "))
        assert(output.head.contains("Connection refused"))


private object AgentClientMainTest:
  private val ExpectedTerminate = ShutDown(Some(SIGTERM))
