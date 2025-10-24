package js7.agent.tests

import cats.effect.ExitCode
import js7.agent.client.main.SubagentClientMain
import js7.agent.client.main.SubagentClientMain.Conf
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.tests.SubagentClientMainTest.*
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString, RichJson}
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class SubagentClientMainTest extends OurAsyncTestSuite, BeforeAndAfterAll, TestAgentProvider:

  override def beforeAll() =
    super.beforeAll()
    dataDirectory / "work" / "http-uri" := (agent.localUri / "subagent").toString
    agent

  override def afterAll() = closer.closeThen(super.afterAll())

  "main" in:
    val output = mutable.Buffer.empty[String]
    val commandJson = json"""{ "TYPE": "ShutDown", "processSignal": "SIGTERM" }"""
    SubagentClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory",
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
    SubagentClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory"),
        o => output += o)
      .map: exitCode =>
        assert(exitCode == ExitCode.Success && output == List("JS7 Agent is responding"))

  "main with Agent URI only checks whether Agent is responding (it is not)" in:
    val port = findFreeTcpPort()
    val output = mutable.Buffer.empty[String]
    SubagentClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory"),
        output += _)
      .map: exitCode =>
        for o <- output do logger.info(s"┃$o")
        assert:
          exitCode == ExitCode.Success
          //exitCode == ExitCode.Error
          //  && output.head.contains("JS7 Agent is not responding: ")
          //  && output.head.contains("Connection refused")


private object SubagentClientMainTest:
  private val logger = Logger[this.type]
  private val ExpectedTerminate = ShutDown(Some(SIGTERM))
