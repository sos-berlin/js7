package js7.base.io.https

import com.typesafe.config.Config
import java.nio.file.Path

/** HTTPS parameters for clients. */
final case class HttpsConfig(
  keyStoreRef: Option[KeyStoreRef],
  trustStoreRefs: Seq[TrustStoreRef]):
  override def toString =
    s"HttpsConfig(keyStoreRef=$keyStoreRef trustStoreRefs=$trustStoreRefs)"

object HttpsConfig:
  val empty = HttpsConfig(None, Nil)

  def fromConfig(config: Config, configDirectory: Path): HttpsConfig =
    HttpsConfig(
      KeyStoreRef
        .clientFromConfig(config, configDirectory = configDirectory)
        .toOption,
      TrustStoreRef.fromConfig(config))
