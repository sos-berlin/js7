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
import js7.common.configuration.Js7Configuration
import scala.jdk.CollectionConverters.*

final case class ClusterWatchConf(
  configDirectory: Path,
  clusterNodeAdmissions: Nel[Admission],
  httpsConfig: HttpsConfig,
  name: String = "ClusterWatch",
  config: Config)

object ClusterWatchConf
{
  def fromCommandLine(args: Seq[String]): ClusterWatchConf =
    CommandLineArguments.parse(args) { a =>
      val configDir = a.as[Path]("--config-directory=").toAbsolutePath

      val config = ConfigFactory.parseMap(Map(
        "js7.config-directory" -> configDir.toString
      ).asJava)
        .withFallback(ConfigFactory.systemProperties)
        .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
        .withFallback(parseConfigIfExists(configDir / "cluster-watch.conf", secret = false))
        .withFallback(Js7Configuration.defaultConfig)
        .resolve

      val uris = a.seqAs[Uri]("--cluster-node-uri=")
      val admissions =
        if (uris.nonEmpty)
          Nel.fromListUnsafe(uris.map(Admission(_)).toList)
        else
          Nel
            .fromList {
              for (cnf <- config.getConfigList("js7.cluster.watch.cluster-nodes").asScala.toList) yield
                Admission(
                  cnf.as[Uri]("uri"),
                  for {
                    userId <- cnf.optionAs[UserId]("user")
                    password <- cnf.optionAs[SecretString]("password")
                  } yield UserAndPassword(userId, password))
            }
            .getOrElse(throw new IllegalArgumentException(
              "Missing Cluster node admissions: js7.cluster.watch.nodes[]"))

      new ClusterWatchConf(
        configDirectory = configDir,
        admissions,
        HttpsConfig.fromConfig(config, configDir),
        config = config)
    }
}
