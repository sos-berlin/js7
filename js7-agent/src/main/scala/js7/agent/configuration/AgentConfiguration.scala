package js7.agent.configuration

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
import js7.agent.data.AgentState
import js7.base.configutils.Configs
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterConf
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.CommonConfiguration
import js7.common.http.configuration.RecouplingStreamReaderConfs
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalLocation
import js7.subagent.configuration.{DirectorConf, SubagentConf}

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  subagentConf: SubagentConf,
  akkaAskTimeout: Timeout,
  clusterConf: ClusterConf,
  name: String)
extends CommonConfiguration
{
  def configDirectory: Path =
    subagentConf.configDirectory

  def dataDirectory: Path =
    subagentConf.dataDirectory

  lazy val stateDirectory: Path =
    dataDirectory / "state"

  def workDirectory: Path =
    subagentConf.workDirectory

  def webServerPorts: Seq[WebServerPort] =
    subagentConf.webServerPorts

  def config: Config =
    subagentConf.config

  implicit def implicitAkkaAskTimeout: Timeout = akkaAskTimeout

  def createDirectories(): Unit = {
    subagentConf.finishAndProvideFiles()
    if (!exists(stateDirectory)) {
      createDirectory(stateDirectory)
    }
  }

  val journalLocation: JournalLocation =
    JournalLocation(AgentState, stateDirectory / "agent" )

  def journalConf: JournalConf =
    clusterConf.journalConf

  val directorConf: DirectorConf =
    DirectorConf(
      journalConf,
      httpsConfig,
      RecouplingStreamReaderConfs.fromConfig(config).orThrow,
      subagentConf)

  // Suppresses Config (which may contain secrets)
  override def toString =
    s"AgentConfiguration($configDirectory,$dataDirectory,$webServerPorts,$name,Config)"
}

object AgentConfiguration
{
  val DefaultName = if (isTest) "Agent" else "JS7"

  val DefaultConfig: Config = Configs
    .loadResource(JavaResource("js7/agent/configuration/agent.conf"))
    .withFallback(SubagentConf.DefaultConfig)

  def fromCommandLine(args: CommandLineArguments, extraConfig: Config = ConfigFactory.empty) = {
    val subagentConf = SubagentConf.fromCommandLine(args,
      extraConfig = extraConfig, internalConfig = DefaultConfig)
    args.requireNoMoreArguments()
    fromDirectories(subagentConf)
  }

  def forTest(
    configAndData: Path,
    name: String,
    extraConfig: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : AgentConfiguration =
    fromDirectories(
      SubagentConf.forTest(
        configAndData = configAndData,
        name = name,
        extraConfig = extraConfig,
        internalConfig = DefaultConfig,
        httpPort = httpPort,
        httpsPort = httpsPort),
      name = name)

  def fromDirectories(
    subagentConf: SubagentConf,
    name: String = DefaultName)
  : AgentConfiguration = {
    import subagentConf.config
    new AgentConfiguration(
      subagentConf,
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      clusterConf = ClusterConf.fromConfig(config).orThrow,
      name = name)
  }
}
