package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.agent.configuration.AgentConfigurationTest._
import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.taskserver.data.DotnetConfiguration
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.createTempDirectory
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
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { dummyDirectoriesConf("-http-port=65536") }
    assert(dummyDirectoriesConf("-http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
  }

  "-https-port=" in {
    // For more tests see CommonConfigurationTest
    assert(dummyDirectoriesConf("-https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234), mutual = false) :: Nil)
  }

  "-log-directory=" in {
    assert(dummyDirectoriesConf().logDirectory == Paths.get("DATA/logs").toAbsolutePath)
    assert(dummyDirectoriesConf("-log-directory=LOGS").logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(dummyDirectoriesConf("-log-directory=test").logDirectory == Paths.get("test").toAbsolutePath)
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
    assert(dummyDirectoriesConf("-rpc-keepalive=5m").rpcKeepaliveDuration == Some(5 * 60.s))
  }

  "-sync-journal" in {
    assert(dummyDirectoriesConf().journalSyncOnCommit)
    assert(dummyDirectoriesConf("-sync-journal").journalSyncOnCommit)
    assert(dummyDirectoriesConf("-sync-journal-").journalSyncOnCommit == false)
  }

  private def dummyDirectoriesConf(args: String*) =
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
