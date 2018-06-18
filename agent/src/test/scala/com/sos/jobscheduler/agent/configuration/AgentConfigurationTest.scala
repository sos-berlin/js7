package com.sos.jobscheduler.agent.configuration

import com.google.common.io.Files.touch
import com.sos.jobscheduler.agent.configuration.AgentConfigurationTest._
import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.taskserver.data.DotnetConfiguration
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectory, createTempDirectory}
import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
 * @author Joacim Zschimmer
 */
final class AgentConfigurationTest extends FreeSpec  {

  private val shellExt = if (isWindows) "cmd" else "sh"

  "Shortest argument list" in {
    provideConfigAndData { (config, data) ⇒
      val c = AgentConfiguration.fromCommandLine(List(s"-config-directory=$config", s"-data-directory=$data")).finishAndProvideFiles
      assert(c.copy(config = ConfigFactory.empty) == AgentConfiguration(
        configDirectory = config,
        dataDirectory = data,
        webServerPorts = Nil,
        workingDirectory = WorkingDirectory,
        logDirectory = data / "logs",
        environment = Map(),
        jobJavaOptions = Nil,
        dotnet = DotnetConfiguration(),
        rpcKeepaliveDuration = None,
        killScript = Some(ProcessKillScript(data / "tmp" / s"kill_task.$shellExt")),
        commandTimeout = 60.s,
        akkaAskTimeout = 60.seconds,
        name = "Agent",
        journalSyncOnCommit = true,
        ConfigFactory.empty))
    }
  }

  "-http-port=" in {
    intercept[IllegalArgumentException] { unfinishedConf("-http-port=65536") }
    assert(unfinishedConf("-http-port=1234"              ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
    assert(unfinishedConf("-http-port=11.22.33.44:1234"  ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("11.22.33.44", 1234)) :: Nil)
    assert(unfinishedConf("-http-port=[1:2:3:4:5:6]:1234").webServerBindings == WebServerBinding.Http(new InetSocketAddress("1:2:3:4:5:6", 1234)) :: Nil)
    assert(unfinishedConf("-http-port=[::1]:1234"        ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("::1", 1234)) :: Nil)
    assert(unfinishedConf("-http-port=1111", "-http-port=2222").webServerBindings ==
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1111)) ::
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 2222)) :: Nil)
  }

  "-https-port=" in {
    provideConfigAndData { (config, data) ⇒
      createDirectory(config / "private")
      touch(config / "private/private-https.jks")
      intercept[IllegalArgumentException] { conf(s"-config-directory=$config", s"-data-directory=$data", "-https-port=65536") }
      assert(conf(s"-config-directory=$config", s"-data-directory=$data", "-https-port=1234").webServerBindings == List(WebServerBinding.Https(
        new InetSocketAddress("0.0.0.0", 1234),
        KeystoreReference(
          url = (config.toAbsolutePath / "private/private-https.jks").toUri.toURL,
          storePassword = Some(SecretString("jobscheduler")),
          keyPassword = Some(SecretString("jobscheduler"))))))
      assert(conf(s"-config-directory=$config", s"-data-directory=$data", "-https-port=11.22.33.44:1234").webServerBindings == List(WebServerBinding.Https(
        new InetSocketAddress("11.22.33.44", 1234),
        KeystoreReference(
          url = (config.toAbsolutePath / "private/private-https.jks").toUri.toURL,
          storePassword = Some(SecretString("jobscheduler")),
          keyPassword = Some(SecretString("jobscheduler"))))))
    }
  }

  "-log-directory=" in {
    assert(unfinishedConf().logDirectory == Paths.get("DATA/logs").toAbsolutePath)
    assert(unfinishedConf("-log-directory=LOGS").logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(unfinishedConf("-log-directory=test").logDirectory == Paths.get("test").toAbsolutePath)
  }

  "-kill-script= is missing (default)" in {
    provideConfigAndData { (config, data) ⇒
      val expectedFile = data / s"tmp/kill_task.$shellExt"
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data").finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(expectedFile)))
    }
  }

  "-kill-script= (empty)" in {
    provideConfigAndData { (config, data) ⇒
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data", "-kill-script=").finishAndProvideFiles
      assert(myConf.killScript == None)
    }
  }

  "-kill-script=FILE" in {
    provideConfigAndData { (config, data) ⇒
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data", "-kill-script=/my/kill/script").finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(Paths.get("/my/kill/script").toAbsolutePath)))
    }
  }

  "-rpc-keepalive=" in {
    assert(unfinishedConf("-rpc-keepalive=5m").rpcKeepaliveDuration == Some(5 * 60.s))
  }

  "-sync-journal" in {
    assert(unfinishedConf().journalSyncOnCommit)
    assert(unfinishedConf("-sync-journal").journalSyncOnCommit)
    assert(unfinishedConf("-sync-journal-").journalSyncOnCommit == false)
  }

  private def unfinishedConf(args: String*) =
    conf(Array("-config-directory=CONFIG", "-data-directory=DATA") ++ args: _*)

  private def conf(args: String*) = AgentConfiguration.fromCommandLine(args.toVector)
}

object AgentConfigurationTest
{
  private def provideConfigAndData(body: (Path, Path) ⇒ Unit): Unit = {
    val config = createTempDirectory("AgentConfigurationTest-config")
    val data = createTempDirectory("AgentConfigurationTest-data")
    try body(config, data)
    finally {
      deleteDirectoryRecursively(config)
      deleteDirectoryRecursively(data)
    }
  }
}
