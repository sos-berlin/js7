package js7.agent.tests

import js7.agent.client.main.AgentClientMain
import js7.agent.command.CommandHandler
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.tests.AgentClientMainTest._
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString, RichJson}
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.problem.Checked
import js7.common.guice.ScalaAbstractModule
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.core.command.CommandMeta
import monix.eval.Task
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class AgentClientMainTest extends AnyFreeSpec with BeforeAndAfterAll with TestAgentProvider
{
  coupleScribeWithSlf4j()

  override def afterAll() = closer closeThen super.afterAll()

  override protected def extraAgentModule = new ScalaAbstractModule {
    override def configure() = {
      bindInstance[CommandHandler](new CommandHandler {
        def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
          Task {
            (command match {
              case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
              case _ => fail()
            })
            .map(_.asInstanceOf[command.Response])
          }

        def overview = throw new NotImplementedError
        def detailed = throw new NotImplementedError
      })
    }
  }

  "main" in {
    val output = mutable.Buffer.empty[String]
    val commandJson = json"""{ "TYPE": "ShutDown", "processSignal": "SIGTERM" }"""
    AgentClientMain.run(
      List(
        s"--data-directory=$dataDirectory",
        agent.localUri.toString,
        commandJson.compactPrint,
        "?"),
      o => output += o)
    assert(output.size == 2)
    assert(output(0).parseJson == Right(json"""{ "TYPE": "Accepted" }"""))
    output(1).parseJsonOrThrow.fieldOrThrow("startedAt")
  }

  "main with Agent URI only checks wether Agent is responding (it is)" in {
    val output = mutable.Buffer.empty[String]
    assertResult(0) {
      AgentClientMain.run(List(s"--data-directory=$dataDirectory", agent.localUri.toString), o => output += o)
    }
    assert(output == List("JS7 Agent is responding"))
  }

  "main with Agent URI only checks wether Agent is responding (it is not)" in {
    val port = findFreeTcpPort()
    val output = mutable.Buffer.empty[String]
    assertResult(1) {
      AgentClientMain.run(List(s"--data-directory=$dataDirectory", s"http://127.0.0.1:$port"), output += _)
    }
    assert(output.head contains "JS7 Agent is not responding: ")
    assert(output.head contains "Connection refused")
  }
}

private object AgentClientMainTest
{
  private val ExpectedTerminate = ShutDown(Some(SIGTERM))
}
