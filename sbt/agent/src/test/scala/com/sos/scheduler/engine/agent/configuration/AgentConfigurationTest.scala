package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.FileUtils._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.sprayutils.WebServerBinding
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.data.DotnetConfiguration
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createTempDirectory, delete}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentConfigurationTest extends FreeSpec {

  private val shellExt = if (isWindows) "cmd" else "sh"

  "Empty argument list" in {
    val c = AgentConfiguration(Nil).finishAndProvideFiles
    assert(c.copy(config = ConfigFactory.empty) == AgentConfiguration(
      dataDirectory = None,
      http = None,
      https = None,
      uriPathPrefix = "",
      externalWebServiceClasses = Nil,
      workingDirectory = WorkingDirectory,
      logDirectory = temporaryDirectory,
      environment = Map(),
      jobJavaOptions = Nil,
      dotnet = DotnetConfiguration(),
      rpcKeepaliveDuration = None,
      killScript = Some(ProcessKillScript(temporaryDirectory / s"kill_task.$shellExt")),
      ConfigFactory.empty))
  }

  "-https-port=" in {
    intercept[IllegalArgumentException] { conf(List("-https-port=1234")).https }
    intercept[IllegalArgumentException] { conf(List("-https-port=1234")).https }
    intercept[IllegalArgumentException] { conf(List("-data-directory=/TEST/DATA", "-https-port=65536")) }
    assert(conf(List("-data-directory=/TEST/DATA", "-https-port=1234")).https == Some(WebServerBinding.Https(
      new InetSocketAddress("0.0.0.0", 1234),
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/private-https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler"))))))
    assert(conf(List("-data-directory=/TEST/DATA", "-https-port=11.22.33.44:1234")).https == Some(WebServerBinding.Https(
      new InetSocketAddress("11.22.33.44", 1234),
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/private-https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler"))))))
  }

  "-http-port=" in {
    intercept[IllegalArgumentException] { conf(List("-http-port=65536")) }
    assert(conf(List("-http-port=1234"              )).http == Some(WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1234))))
    assert(conf(List("-http-port=11.22.33.44:1234"  )).http == Some(WebServerBinding.Http(new InetSocketAddress("11.22.33.44", 1234))))
    assert(conf(List("-http-port=[1:2:3:4:5:6]:1234")).http == Some(WebServerBinding.Http(new InetSocketAddress("1:2:3:4:5:6", 1234))))
    assert(conf(List("-http-port=[::1]:1234"        )).http == Some(WebServerBinding.Http(new InetSocketAddress("::1", 1234))))
  }
  "-log-directory=" in {
    assert(conf(List("-data-directory=TEST/DATA")).logDirectory == Paths.get("TEST/DATA/logs").toAbsolutePath)
    assert(conf(List("-data-directory=TEST/DATA", "-log-directory=LOGS")).logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(conf(List("-log-directory=test")).logDirectory == Paths.get("test").toAbsolutePath)
  }

  "-uri-prefix=" in {
    assert(conf(List("-uri-prefix=test")).uriPathPrefix == "test")
    assert(conf(List("-uri-prefix=/test/")).uriPathPrefix == "test")
  }

  "-kill-script= is missing (default)" - {
    "Without -data-directory" in {
      assert(conf(Nil).logDirectory == conf(Nil).temporaryDirectory)
      assert(conf(Nil).logDirectory == temporaryDirectory)
      val generatedFile = conf(Nil).logDirectory / s"kill_task.$shellExt"
      assert(AgentConfiguration(List("-http-port=11111")).finishAndProvideFiles.killScript == Some(ProcessKillScript(generatedFile)))
      delete(generatedFile)
    }

    "With -data-directory" in {
      val data = createTempDirectory("AgentConfigurationTest-")
      val expectedFile = data / s"tmp/kill_task.$shellExt"
      val myConf = conf(List(s"-data-directory=$data")).finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(expectedFile)))
      delete(expectedFile)
      delete(data / "logs")
      delete(data / "tmp")
      delete(data)
    }
  }

  "-kill-script= (empty)" - {
    "Without -data-directory" in {
      val myConf = conf(List("-kill-script=")).finishAndProvideFiles
      assert(myConf.killScript == None)
    }

    "With -data-directory" in {
      val data = createTempDirectory("AgentConfigurationTest-")
      val myConf = conf(List(s"-data-directory=$data", "-kill-script=")).finishAndProvideFiles
      assert(myConf.killScript == None)
      delete(data / "logs")
      delete(data / "tmp")
      delete(data)
    }
  }

  "-kill-script=FILE" - {
    val data = createTempDirectory("AgentConfigurationTest-")
    val myConf = conf(List(s"-data-directory=$data", "-kill-script=/my/kill/script")).finishAndProvideFiles
    assert(myConf.killScript == Some(ProcessKillScript(Paths.get("/my/kill/script").toAbsolutePath)))
    delete(data / "logs")
    delete(data / "tmp")
    delete(data)
  }

  "-rpc-keepalive=" in {
    assert(conf(List("-rpc-keepalive=5m")).rpcKeepaliveDuration == Some(5 * 60.s))
  }

  private def conf(args: List[String]) = AgentConfiguration(args)
}
