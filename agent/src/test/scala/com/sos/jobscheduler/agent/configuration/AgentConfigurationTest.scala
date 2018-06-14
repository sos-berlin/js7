package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.taskserver.data.DotnetConfiguration
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createTempDirectory, delete}
import java.nio.file.Paths
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration.DurationInt

/**
 * @author Joacim Zschimmer
 */
final class AgentConfigurationTest extends FreeSpec with BeforeAndAfterAll {

  private val shellExt = if (isWindows) "cmd" else "sh"
  private lazy val tmp = createTempDirectory("test-")

  override def afterAll() = {
    deleteDirectoryRecursively(tmp)
    super.afterAll()
  }

  "Empty argument list" in {
    val c = AgentConfiguration(List(s"-data-directory=$tmp")).finishAndProvideFiles
    assert(c.copy(config = ConfigFactory.empty, dataDirectory = Some(tmp)) == AgentConfiguration(
      dataDirectory = Some(tmp),
      configDirectory = Some(tmp / "config"),
      webServerBindings = Nil,
      workingDirectory = WorkingDirectory,
      logDirectory = tmp / "logs",
      environment = Map(),
      jobJavaOptions = Nil,
      dotnet = DotnetConfiguration(),
      rpcKeepaliveDuration = None,
      killScript = Some(ProcessKillScript(tmp / "tmp" / s"kill_task.$shellExt")),
      commandTimeout = 60.s,
      akkaAskTimeout = 60.seconds,
      name = "Agent",
      journalSyncOnCommit = true,
      ConfigFactory.empty))
  }

  "-https-port=" in {
    intercept[IllegalArgumentException] { conf("-https-port=1234") }
    intercept[IllegalArgumentException] { conf("-data-directory=/TEST/DATA", "-https-port=65536") }
    assert(conf("-data-directory=/TEST/DATA", "-https-port=1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("0.0.0.0", 1234),
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/private-https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler"))))))
    assert(conf("-data-directory=/TEST/DATA", "-https-port=11.22.33.44:1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("11.22.33.44", 1234),
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/private-https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler"))))))
  }

  "-http-port=" in {
    intercept[IllegalArgumentException] { conf("-http-port=65536") }
    assert(conf("-http-port=1234"              ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
    assert(conf("-http-port=11.22.33.44:1234"  ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("11.22.33.44", 1234)) :: Nil)
    assert(conf("-http-port=[1:2:3:4:5:6]:1234").webServerBindings == WebServerBinding.Http(new InetSocketAddress("1:2:3:4:5:6", 1234)) :: Nil)
    assert(conf("-http-port=[::1]:1234"        ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("::1", 1234)) :: Nil)
    assert(conf("-http-port=1111", "-http-port=2222").webServerBindings ==
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1111)) ::
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 2222)) :: Nil)
  }

  "-log-directory=" in {
    assert(conf("-data-directory=TEST/DATA").logDirectory == Paths.get("TEST/DATA/logs").toAbsolutePath)
    assert(conf("-data-directory=TEST/DATA", "-log-directory=LOGS").logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(conf("-log-directory=test").logDirectory == Paths.get("test").toAbsolutePath)
  }

  "-kill-script= is missing (default)" - {
    val data = createTempDirectory("AgentConfigurationTest-")
    val expectedFile = data / s"tmp/kill_task.$shellExt"
    val myConf = conf(s"-data-directory=$data").finishAndProvideFiles
    assert(myConf.killScript == Some(ProcessKillScript(expectedFile)))
    delete(expectedFile)
    delete(data / "logs")
    delete(data / "state")
    delete(data / "tmp")
    delete(data)
  }

  "-kill-script= (empty)" - {
    val data = createTempDirectory("AgentConfigurationTest-")
    val myConf = conf(s"-data-directory=$data", "-kill-script=").finishAndProvideFiles
    assert(myConf.killScript == None)
    delete(data / "logs")
    delete(data / "state")
    delete(data / "tmp")
    delete(data)
  }

  "-kill-script=FILE" in {
    val data = createTempDirectory("AgentConfigurationTest-")
    val myConf = conf(s"-data-directory=$data", "-kill-script=/my/kill/script").finishAndProvideFiles
    assert(myConf.killScript == Some(ProcessKillScript(Paths.get("/my/kill/script").toAbsolutePath)))
    delete(data / "logs")
    delete(data / "state")
    delete(data / "tmp")
    delete(data)
  }

  "-rpc-keepalive=" in {
    assert(conf("-rpc-keepalive=5m").rpcKeepaliveDuration == Some(5 * 60.s))
  }

  "-sync-journal" in {
    assert(conf().journalSyncOnCommit)
    assert(conf("-sync-journal").journalSyncOnCommit)
    assert(conf("-sync-journal-").journalSyncOnCommit == false)
  }

  private def conf(args: String*) = AgentConfiguration(args.toVector)
}
