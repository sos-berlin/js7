package js7.tests

import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.client.main.ControllerClientMain
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class ControllerClientMainTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val versionedItems = Nil
  private def configDirectory = directoryProvider.controller.configDir
  private def dataDirectory = directoryProvider.controller.dataDir
  private val httpsPort = findFreeTcpPort()
  override protected lazy val controllerHttpPort = None
  override protected lazy val controllerHttpsPort = Some(httpsPort)

  "https://" in {
    assert(controller.localUri.string startsWith "https://")
  }

  "main with Controller URI only checks wether Controller is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      ControllerClientMain.run(
        s"--config-directory=$configDirectory" :: s"--data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" :: Nil,
        output += _)
    }
    assert(output == List("JS7 Controller is responding"))
  }

  "Multiple api calls" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      ControllerClientMain.run(
        s"--config-directory=$configDirectory" :: s"--data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" ::
          "?" :: "/order" :: Nil,
        output += _)
    }
    assert(output(0) contains "version:")
    assert(output(1) == "---")
    assert(output(2) contains "count: 0")
  }

  "main with Controller URI only checks wether Controller is responding (it is not)" in {
    val unusedPort = 0
    val output = mutable.Buffer[String]()
    assertResult(1) {
      ControllerClientMain.run(
        s"--config-directory=$configDirectory" :: s"--data-directory=$dataDirectory" ::
          s"https://localhost:$unusedPort" ::
          Nil,
        output += _)
    }
    assert(output.head contains "JS7 Controller is not responding: ")
    //assert(output.head contains "Connection refused")
  }

  "ShutDown responds with Accepted" in {
    // May fail on slow computer if web server terminates before responding !!!
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: ShutDown }"""
    try {
      ControllerClientMain.run(
        s"--config-directory=$configDirectory" :: s"--data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" ::
          commandYaml :: Nil,
        output += _)
      assert(output == List("TYPE: Accepted"))
    } catch {
      case t: akka.stream.StreamTcpException if t.getMessage contains "Connection reset by peer" =>
    }
    controller.terminated await 99.s
  }
}
