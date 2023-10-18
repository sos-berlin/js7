package js7.base.io.https

import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import scala.jdk.CollectionConverters.*

/** To trust the server's certificate.
  * @author Joacim Zschimmer
  */
final case class TrustStoreRef(
  url: URL,
  /** Password for file */
  storePassword: SecretString)
extends StoreRef:

  override def toString = s"TrustStore $url"


object TrustStoreRef:
  private val configKey = "js7.web.https.truststores"

  def apply(file: Path, storePassword: SecretString): TrustStoreRef =
    new TrustStoreRef(file.toAbsolutePath.toUri.toURL, storePassword)

  def fromConfig(config: Config): Seq[TrustStoreRef] =
    if !config.hasPath(configKey) then
      Nil
    else
      config.getObjectList(configKey)
        .asScala
        .toVector
        .map(obj =>
          TrustStoreRef(
            file = obj.toConfig.as[Path]("file"),
            storePassword =
              obj.toConfig.as[SecretString]("store-password", SecretString.empty)))

  def fromKeyStore(keyStoreRef: KeyStoreRef): TrustStoreRef =
    new TrustStoreRef(keyStoreRef.url, keyStoreRef.storePassword)
