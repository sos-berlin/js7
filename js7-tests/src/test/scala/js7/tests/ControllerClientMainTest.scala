package js7.tests

import cats.effect.ExitCode
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString, RichJson}
import js7.base.test.OurAsyncTestSuite
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.client.main.ControllerClientMain
import js7.controller.client.main.ControllerClientMain.Conf
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class ControllerClientMainTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Nil
  protected val items = Nil
  private def configDirectory = directoryProvider.controllerEnv.configDir
  private def dataDirectory = directoryProvider.controllerEnv.dataDir
  private val httpsPort = findFreeTcpPort()
  override protected lazy val controllerHttpPort = None
  override protected lazy val controllerHttpsPort = Some(httpsPort)

  "is https://" in:
    assert(controller.localUri.string.startsWith("https://"))

  "main with Controller URI only checks whether Controller is responding (it is)" in:
    val output = mutable.Buffer[String]()
    ControllerClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory",
          s"https://localhost:$httpsPort"),
        output += _)
      .map: exitCode =>
        assert(exitCode == ExitCode.Success && output == List("JS7 Controller is responding"))

  "Multiple api calls" in:
    val output = mutable.Buffer[String]()
    ControllerClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory",
          s"https://localhost:$httpsPort",
          "?",
          "/order"),
        output += _)
      .map: exitCode =>
        assert(exitCode == ExitCode.Success)
        assert(output(0).contains("\"version\":"))
        assert(output(1).contains("\"count\": 0"))

  "main with Controller URI only checks whether Controller is responding (it is not)" in:
    val unusedPort = 0
    val output = mutable.Buffer[String]()
    ControllerClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory",
          s"https://localhost:$unusedPort"),
        output += _)
      .map: exitCode =>
        assert(exitCode == ExitCode.Error)
        assert(output.head.contains("JS7 Controller is not responding: "))
        //assert(output.head contains "Connection refused")

  "ShutDown responds with Accepted" in:
    // May fail on slow computer if web server terminates before responding !!!
    val output = mutable.Buffer[String]()
    val commandJson = json"""{ "TYPE": "ShutDown" }"""
    ControllerClientMain
      .program(
        Conf.args(
          s"--config-directory=$configDirectory",
          s"--data-directory=$dataDirectory",
          s"https://localhost:$httpsPort",
          commandJson.compactPrint),
        output += _)
      .map: exitCode =>
        assert(exitCode == ExitCode.Success)
        assert(output.map(_.parseJsonOrThrow) == List(json"""{ "TYPE": "Accepted" }"""))
      .flatMap: _ =>
        controller.untilTerminated.as(succeed)
    //catch
    //  case t: pekko.stream.StreamTcpException if t.getMessage contains "Connection reset by peer" =>
