package js7.tests

import js7.base.time.ScalaTime._
import js7.common.scalautil.Futures.implicits._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.master.client.main.MasterClientMain
import js7.tests.testenv.MasterAgentForScalaTest
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class MasterClientMainTest extends AnyFreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest {

  protected val agentRefPaths = Nil
  protected val fileBased = Nil
  private def configDirectory = directoryProvider.master.configDir
  private def dataDirectory = directoryProvider.master.dataDir
  private val httpsPort = findFreeTcpPort()
  override protected lazy val masterHttpPort = None
  override protected lazy val masterHttpsPort = Some(httpsPort)

  override def beforeAll() = {
    directoryProvider.master.provideHttpsCertificate()
    assert(master.localUri.string startsWith "https://")
  }

  "main with Master URI only checks wether Master is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      MasterClientMain.run(
        s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" :: Nil,
        output += _)
    }
    assert(output == List("JobScheduler Master is responding"))
  }

  "Multiple api calls" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      MasterClientMain.run(
        s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" ::
          "?" :: "/order" :: Nil,
        output += _)
    }
    assert(output(0) contains "version:")
    assert(output(1) == "---")
    assert(output(2) contains "count: 0")
  }

  "main with Master URI only checks wether Master is responding (it is not)" in {
    val unusedPort = 0
    val output = mutable.Buffer[String]()
    assertResult(1) {
      MasterClientMain.run(
        s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
          s"https://localhost:$unusedPort" ::
          Nil,
        output += _)
    }
    assert(output.head contains "JobScheduler Master is not responding: ")
    //assert(output.head contains "Connection refused")
  }

  "ShutDown" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: ShutDown }"""
    MasterClientMain.run(
      s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
        s"https://localhost:$httpsPort" ::
        commandYaml :: Nil,
      output += _)
    assert(output == List("TYPE: Accepted"))
    master.terminated await 99.s
  }
}
