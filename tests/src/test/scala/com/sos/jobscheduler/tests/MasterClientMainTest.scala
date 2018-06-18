package com.sos.jobscheduler.tests

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.client.main.MasterClientMain
import com.sos.jobscheduler.tests.MasterClientMainTest._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class MasterClientMainTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  protected val agentPaths = Nil
  private def configDirectory = directoryProvider.master.config
  private def dataDirectory = directoryProvider.master.data
  private val httpsPort = findRandomFreeTcpPort()
  override protected lazy val masterHttpPort = None
  override protected lazy val masterHttpsPort = Some(httpsPort)

  override def beforeAll() = {
    PrivateHttpJksResource copyToFile configDirectory / "private/private-https.jks"
    PublicHttpJksResource copyToFile configDirectory / "public-https.jks"
    super.beforeAll()
  }

  "main with Master URI only checks wether Master is responding (it is)" in {
    assert(master.localUri.scheme == "https")
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
    val port = findRandomFreeTcpPort()
    val output = mutable.Buffer[String]()
    assertResult(1) {
      MasterClientMain.run(
        s"-config-directory=$configDirectory" :: s"-data-directory=$dataDirectory" ::
          s"https://localhost:$port" ::
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

object MasterClientMainTest {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=master-https -config-directory=tests/src/test/resources/com/sos/jobscheduler/tests/config/
  private val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/tests/config/private/private-https.jks")
  private val PublicHttpJksResource = JavaResource("com/sos/jobscheduler/tests/config/public-https.jks")
}
