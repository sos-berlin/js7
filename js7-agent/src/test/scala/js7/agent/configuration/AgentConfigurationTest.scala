package js7.agent.configuration

import java.net.InetSocketAddress
import java.nio.file.Files.createTempDirectory
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration.DefaultConfig
import js7.agent.configuration.AgentConfigurationTest.*
import js7.base.configutils.Configs.*
import js7.base.convert.As.StringAsBoolean
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.ClusterConf
import js7.common.commandline.CommandLineArguments
import js7.common.pekkohttp.web.data.WebServerPort
import js7.launcher.configuration.ProcessKillScript
import js7.subagent.configuration.SubagentConf
import scala.concurrent.duration.*

/**
 * @author Joacim Zschimmer
 */
final class AgentConfigurationTest extends OurTestSuite:
  "Shortest argument list" in:
    provideConfigAndData { (configDir, dataDir) =>
      val agentConf = AgentConfiguration.fromCommandLine(CommandLineArguments(Seq(
        s"--config-directory=$configDir",
        s"--data-directory=$dataDir")))
      agentConf.createDirectories()
      assert(agentConf == {
        val subagentConf = SubagentConf.of(
          configDirectory = configDir,
          dataDirectory = dataDir,
          logDirectory = dataDir / "logs",
          jobWorkingDirectory = WorkingDirectory,
          webServerPorts = Nil,
          killScript = Some(ProcessKillScript(dataDir / "work" / "kill_task.sh")),
          internalConfig = DefaultConfig)
        AgentConfiguration(
          subagentConf,
          pekkoAskTimeout = 1.hour,
          clusterConf = {
            val clusterConf = ClusterConf
              .fromConfig(subagentConf.config)
              .orThrow
            clusterConf.copy(
              journalConf = clusterConf.journalConf.copy(
                slowCheckState = sys.props.get("js7.test").fold(false)(StringAsBoolean(_))))
          },
          name = AgentConfiguration.DefaultName)
      })
    }

  "--http-port=" in:
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { dummyDirectoriesConf("--http-port=65536") }
    assert(dummyDirectoriesConf("--http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "--https-port=" in:
    // For more tests see CommonConfigurationTest
    assert(dummyDirectoriesConf("--https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "System property" in:
    assert(dummyDirectoriesConf().config.getString("user.name") == sys.props("user.name"))

  private def dummyDirectoriesConf(args: String*) =
    conf((List("--config-directory=CONFIG", "--data-directory=DATA") ++ args)*)

  private def conf(args: String*) =
    AgentConfiguration.fromCommandLine(
      CommandLineArguments(args.toVector),
      config"user.name = AgentConfigurationTest"/*Will be overridden*/)


object AgentConfigurationTest:
  private def provideConfigAndData(body: (Path, Path) => Unit): Unit =
    val config = createTempDirectory("AgentConfigurationTest-config")
    val data = createTempDirectory("AgentConfigurationTest-data")
    try body(config, data)
    finally
      deleteDirectoryRecursively(config)
      deleteDirectoryRecursively(data)
