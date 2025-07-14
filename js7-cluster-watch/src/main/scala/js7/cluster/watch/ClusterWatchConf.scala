package js7.cluster.watch

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs
import js7.base.configutils.Configs.{ConvertibleConfig, parseConfigIfExists}
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.pekkohttp.web.data.WebServerPort
import js7.data.cluster.ClusterWatchId
import scala.jdk.CollectionConverters.*

final case class ClusterWatchConf(
  configDirectory: Path,
  clusterWatchId: ClusterWatchId,
  clusterNodeAdmissions: Nel[Admission],
  webServerPorts: Seq[WebServerPort],
  config: Config)
extends CommonConfiguration:
  val name = "ClusterWatch"


object ClusterWatchConf:
  private val clusterWatchDefaultConfig: Config = Configs.loadResource:
    JavaResource("js7/cluster/watch/configuration/cluster-watch.conf")

  def fromCommandLine(args: CommandLineArguments): ClusterWatchConf =
    val configDir = args.as[Path]("--config-directory=").toAbsolutePath
    args.as[Path]("--data-directory=").toAbsolutePath // not used
    val clusterWatchId = args.as[ClusterWatchId]("--cluster-watch-id=")
    val common = CommonConfiguration.Common.fromCommandLineArguments(args)
    import common.configDirectory

    val config = ConfigFactory.parseMap(Map(
      "js7.config-directory" -> configDirectory.toString
    ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(parseConfigIfExists(configDirectory / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "cluster-watch.conf", secret = false))
      .withFallback(clusterWatchDefaultConfig)
      .withFallback(Js7Configuration.defaultConfig)
      .resolve

    val uris = args.seqAs[Uri]("--cluster-node-uri=")
    val admissions =
      if uris.nonEmpty then
        Nel.fromListUnsafe(uris.map(Admission(_)).toList)
      else
        Nel.fromList:
          config.getConfigList("js7.journal.cluster.watch.cluster-nodes")
            .asInstanceOf[java.util.List[Config]] // Due to Scala 3.5.2 -Yexplicit-nulls
            .asScala.toList
            .map: cnf =>
              Admission(
                cnf.as[Uri]("uri"),
                for
                  userId <- cnf.optionAs[UserId]("user")
                  password <- cnf.optionAs[SecretString]("password")
                yield
                  UserAndPassword(userId, password))
        .getOrElse:
          throw new IllegalArgumentException(
            "Missing Cluster node admissions: js7.cluster.watch.nodes[]")

    new ClusterWatchConf(
      configDirectory = configDirectory,
      clusterWatchId,
      admissions,
      common.webServerPorts,
      config = config)
