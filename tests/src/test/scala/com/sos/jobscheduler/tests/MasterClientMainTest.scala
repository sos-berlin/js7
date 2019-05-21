package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.master.client.main.MasterClientMain
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class MasterClientMainTest extends FreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest {

  protected val agentRefPaths = Nil
  protected val fileBased = Nil
  private def configDirectory = directoryProvider.master.config
  private def dataDirectory = directoryProvider.master.data
  private val httpsPort = findFreeTcpPort()
  override protected lazy val masterHttpPort = None
  override protected lazy val masterHttpsPort = Some(httpsPort)

  override def beforeAll() = {
    directoryProvider.master.provideHttpsCertificate()
    assert(master.localUri.scheme == "https")
  }

  "main with Master URI only checks wether Master is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      MasterClientMain.run(
        s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
          s"https://localhost:$httpsPort" :: Nil,
        output.+=)
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
        output.+=)
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

  "Terminate" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: Terminate }"""
    MasterClientMain.run(
      s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
        s"https://localhost:$httpsPort" ::
        commandYaml :: Nil,
      output.+=)
    assert(output == List("TYPE: Accepted"))
    master.terminated await 99.s
  }
}
