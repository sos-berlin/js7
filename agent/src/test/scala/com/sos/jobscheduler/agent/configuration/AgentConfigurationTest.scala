package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.agent.configuration.AgentConfiguration.DefaultConfig
import com.sos.jobscheduler.agent.configuration.AgentConfigurationTest._
import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.core.event.journal.JournalConf
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
    provideConfigAndData { (config, data) =>
      val c = AgentConfiguration.fromCommandLine(CommandLineArguments(s"-config-directory=$config" :: s"-data-directory=$data" :: Nil))
        .finishAndProvideFiles
      assert(c.copy(config = DefaultConfig) == AgentConfiguration(
        configDirectory = config,
        dataDirectory = data,
        webServerPorts = Nil,
        logDirectory = data / "logs",
        jobWorkingDirectory = WorkingDirectory,
        jobJavaOptions = Nil,
        killScript = Some(ProcessKillScript(data / "tmp" / s"kill_task.$shellExt")),
        akkaAskTimeout = 60.seconds,
        JournalConf.fromConfig(DefaultConfig),
        name = AgentConfiguration.DefaultName,
        DefaultConfig))
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

  "-job-working-directory=" in {
    assert(dummyDirectoriesConf("-job-working-directory=DIR").jobWorkingDirectory == Paths.get("DIR").toAbsolutePath)
  }

  "-kill-script= is missing (default)" in {
    provideConfigAndData { (config, data) =>
      val expectedFile = data / s"tmp/kill_task.$shellExt"
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data").finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(expectedFile)))
    }
  }

  "-kill-script= (empty)" in {
    provideConfigAndData { (config, data) =>
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data", "-kill-script=").finishAndProvideFiles
      assert(myConf.killScript == None)
    }
  }

  "-kill-script=FILE" in {
    provideConfigAndData { (config, data) =>
      val myConf = conf(s"-config-directory=$config", s"-data-directory=$data", "-kill-script=/my/kill/script").finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(Paths.get("/my/kill/script").toAbsolutePath)))
    }
  }

  "System property" in {
    assert(dummyDirectoriesConf().config.getString("user.name") == sys.props("user.name"))
  }

  private def dummyDirectoriesConf(args: String*) =
    conf(Array("-config-directory=CONFIG", "-data-directory=DATA") ++ args: _*)

  private def conf(args: String*) = {
    AgentConfiguration.fromCommandLine(
      CommandLineArguments(args.toVector),
      ConfigFactory.parseString("user.name = MasterConfigurationTest"/*Will be overridden*/))
  }
}

object AgentConfigurationTest
{
  private def provideConfigAndData(body: (Path, Path) => Unit): Unit = {
    val config = createTempDirectory("AgentConfigurationTest-config")
    val data = createTempDirectory("AgentConfigurationTest-data")
    try body(config, data)
    finally {
      deleteDirectoryRecursively(config)
      deleteDirectoryRecursively(data)
    }
  }
}
