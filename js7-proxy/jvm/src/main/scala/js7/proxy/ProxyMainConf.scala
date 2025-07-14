package js7.proxy

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
import js7.proxy.configuration.{ProxyConf, ProxyConfs}
import scala.jdk.CollectionConverters.*

private final case class ProxyMainConf(
  configDirectory: Path,
  admissions: Nel[Admission],
  httpsConfig: HttpsConfig,
  proxyConf: ProxyConf,
  clusterWatchId: Option[ClusterWatchId],
  config: Config)
extends BasicConfiguration:

  val name = "Proxy"


private object ProxyMainConf:

  def fromCommandLine(args: CommandLineArguments): ProxyMainConf =
    val configDir = args.as[Path]("--config-directory=").toAbsolutePath
    val unusedDataDir = args.as[Path]("--data-directory=").toAbsolutePath
    val clusterWatchId = args.optionAs[ClusterWatchId]("--cluster-watch-id=")

    val config = ConfigFactory.parseMap(Map(
      "js7.config-directory" -> configDir.toString
    ).asJava)
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(parseConfigIfExists(configDir / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDir / "proxy.conf", secret = false))
      .withFallback(Js7Configuration.defaultConfig)
      .resolve

    val uris = args.seqAs[Uri]("--cluster-node-uri=")
    val admissions =
      if uris.nonEmpty then
        Nel.fromListUnsafe(uris.map(Admission(_)).toList)
      else
        Nel.fromList:
          config.getConfigList("js7.proxy.cluster-nodes")
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

    ProxyMainConf(
      configDirectory = configDir,
      admissions,
      HttpsConfig.fromConfig(config, configDir),
      ProxyConfs.default,
      clusterWatchId,
      config = config)
