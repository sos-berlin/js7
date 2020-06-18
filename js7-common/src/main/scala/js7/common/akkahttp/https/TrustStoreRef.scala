package js7.common.akkahttp.https

import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.common.configutils.Configs._
import scala.jdk.CollectionConverters._

/** To trust the server's certificate.
  * @author Joacim Zschimmer
  */
final case class TrustStoreRef(
  url: URL,
  /** Password for file */
  storePassword: SecretString)
extends StoreRef
{
  override def toString = s"TrustStore $url"
}

object TrustStoreRef
{
  private val configKey = "js7.https.truststores"

  def apply(file: Path, password: SecretString): TrustStoreRef =
    new TrustStoreRef(file.toAbsolutePath.toUri.toURL, password)

  def fromConfig(config: Config): Seq[TrustStoreRef] =
    if (!config.hasPath(configKey))
      Nil
    else
      config.getObjectList(configKey)
        .asScala
        .toVector
        .map(obj =>
          TrustStoreRef(
            file = obj.toConfig.as[Path]("file"),
            password = obj.toConfig.as[SecretString]("store-password")))

  def fromKeyStore(keyStoreRef: KeyStoreRef): TrustStoreRef =
    new TrustStoreRef(keyStoreRef.url, keyStoreRef.storePassword)
}
