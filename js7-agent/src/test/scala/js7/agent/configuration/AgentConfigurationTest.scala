package js7.agent.configuration

import java.net.InetSocketAddress
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Path, Paths}
import js7.agent.configuration.AgentConfiguration.DefaultConfig
import js7.agent.configuration.AgentConfigurationTest.*
import js7.base.configutils.Configs.*
import js7.base.convert.As.StringAsBoolean
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.common.commandline.CommandLineArguments
import js7.common.pekkohttp.web.data.WebServerPort
import js7.journal.configuration.JournalConf
import js7.launcher.configuration.ProcessKillScript
import scala.concurrent.duration.*

/**
 * @author Joacim Zschimmer
 */
final class AgentConfigurationTest extends OurTestSuite
{
  private val shellExt = if (isWindows) "cmd" else "sh"

  "Shortest argument list" in {
    provideConfigAndData { (config, data) =>
      val c = AgentConfiguration
        .fromCommandLine(CommandLineArguments(Seq(
          s"--config-directory=$config",
          s"--data-directory=$data")))
        .finishAndProvideFiles
      assert(c.copy(config = DefaultConfig) == AgentConfiguration(
        configDirectory = config,
        dataDirectory = data,
        webServerPorts = Nil,
        logDirectory = data / "logs",
        jobWorkingDirectory = WorkingDirectory,
        killScript = Some(ProcessKillScript(""))/*unused, replaced by SubagentConf.killScript*/,
        pekkoAskTimeout = 1.hour,
        journalConf = JournalConf.fromConfig(DefaultConfig)
          .copy(slowCheckState = sys.props.get("js7.test").fold(false)(StringAsBoolean(_))),
        name = AgentConfiguration.DefaultName,
        config = DefaultConfig))
    }
  }

  "--http-port=" in {
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { dummyDirectoriesConf("--http-port=65536") }
    assert(dummyDirectoriesConf("--http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
  }

  "--https-port=" in {
    // For more tests see CommonConfigurationTest
    assert(dummyDirectoriesConf("--https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
  }

  "--log-directory=" in {
    assert(dummyDirectoriesConf().logDirectory == Paths.get("DATA/logs").toAbsolutePath)
    assert(dummyDirectoriesConf("--log-directory=LOGS").logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(dummyDirectoriesConf("--log-directory=test").logDirectory == Paths.get("test").toAbsolutePath)
  }

  "--job-working-directory=" in {
    assert(dummyDirectoriesConf("--job-working-directory=DIR").jobWorkingDirectory == Paths.get("DIR").toAbsolutePath)
  }

  "--kill-script= is missing (default)" in {
    provideConfigAndData { (config, data) =>
      val expectedFile = data / s"work/kill_task.$shellExt"
      val myConf = conf(s"--config-directory=$config", s"--data-directory=$data")
        .finishAndProvideFiles
      assert(myConf.subagentConf.killScript == Some(ProcessKillScript(expectedFile)))
    }
  }

  "--kill-script= (empty)" in {
    provideConfigAndData { (config, data) =>
      val myConf = conf(s"--config-directory=$config", s"--data-directory=$data", "--kill-script=")
        .finishAndProvideFiles
      assert(myConf.killScript == None)
    }
  }

  "--kill-script=FILE" in {
    provideConfigAndData { (config, data) =>
      val myConf = conf(s"--config-directory=$config", s"--data-directory=$data",
        "--kill-script=/my/kill/script"
      ).finishAndProvideFiles
      assert(myConf.killScript == Some(ProcessKillScript(Paths.get("/my/kill/script").toAbsolutePath)))
    }
  }

  "System property" in {
    assert(dummyDirectoriesConf().config.getString("user.name") == sys.props("user.name"))
  }

  private def dummyDirectoriesConf(args: String*) =
    conf((List("--config-directory=CONFIG", "--data-directory=DATA") ++ args)*)

  private def conf(args: String*) = {
    AgentConfiguration.fromCommandLine(
      CommandLineArguments(args.toVector),
      config"user.name = ControllerConfigurationTest"/*Will be overridden*/)
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
