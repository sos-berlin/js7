package js7.cluster.watch

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.{ConvertibleConfig, parseConfigIfExists}
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.HttpsConfig
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{BasicConfiguration, Js7Configuration}
import js7.data.cluster.ClusterWatchId
import scala.jdk.CollectionConverters.*

final case class ClusterWatchConf(
  configDirectory: Path,
  clusterWatchId: ClusterWatchId,
  clusterNodeAdmissions: Nel[Admission],
  httpsConfig: HttpsConfig,
  config: Config)
extends BasicConfiguration:
  val name = "ClusterWatch"


object ClusterWatchConf:
  def fromCommandLine(args: CommandLineArguments): ClusterWatchConf =
    val configDir = args.as[Path]("--config-directory=").toAbsolutePath
    args.as[Path]("--data-directory=").toAbsolutePath // not used
    val clusterWatchId = args.as[ClusterWatchId]("--cluster-watch-id=")

    val config = ConfigFactory.parseMap(Map(
      "js7.config-directory" -> configDir.toString
    ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDir / "cluster-watch.conf", secret = false))
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
      configDirectory = configDir,
      clusterWatchId,
      admissions,
      HttpsConfig.fromConfig(config, configDir),
      config = config)
