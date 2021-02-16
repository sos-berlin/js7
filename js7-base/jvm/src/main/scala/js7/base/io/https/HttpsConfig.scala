package js7.base.io.https

import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.log.Logger
import js7.base.problem.Checked._

final case class HttpsConfig(
  keyStoreRef: Option[KeyStoreRef],
  trustStoreRefs: Seq[TrustStoreRef])

object HttpsConfig
{
  val empty = HttpsConfig(None, Nil)
  private val logger = Logger[this.type]

  def fromConfig(config: Config, configDirectory: Path): HttpsConfig =
    HttpsConfig(
      KeyStoreRef.fromConfig(config, default = configDirectory resolve "private/https-keystore.p12")
        .onProblem(p => logger.debug(s"No keystore: $p"))/*TODO Ignore error?*/,
      TrustStoreRef.fromConfig(config))
}
