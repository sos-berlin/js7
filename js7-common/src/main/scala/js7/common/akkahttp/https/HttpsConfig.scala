package js7.common.akkahttp.https

import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.problem.Checked._
import js7.common.scalautil.Logger
import scala.collection.immutable.Seq

final case class HttpsConfig(
  keyStoreRef: Option[KeyStoreRef],
  trustStoreRefs: Seq[TrustStoreRef])

object HttpsConfig
{
  private val logger = Logger(getClass)
  val empty = HttpsConfig(None, Nil)

  def fromConfig(config: Config, configDirectory: Path): HttpsConfig =
    HttpsConfig(
      KeyStoreRef.fromConfig(config, default = configDirectory resolve "private/https-keystore.p12")
        .onProblem(p => logger.debug(s"No keystore: $p"))/*TODO Ignore error?*/,
      TrustStoreRef.fromConfig(config))
}
