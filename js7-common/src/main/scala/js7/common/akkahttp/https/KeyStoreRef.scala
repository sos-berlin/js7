package js7.common.akkahttp.https

import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.common.configutils.Configs._

/** To provide a client certificate to server.
  * @author Joacim Zschimmer
  */
final case class KeyStoreRef(
  url: URL,
  /** Password for file */
  storePassword: SecretString,
  /** PKCS#12 key password */
  keyPassword: SecretString)
extends StoreRef
{
  override def toString = s"KeyStore $url"
}

object KeyStoreRef
{
  def apply(
    file: Path,
    /** Password for file */
    storePassword: SecretString,
    /** PKCS#12 key password */
    keyPassword: SecretString)
  = new KeyStoreRef(file.toUri.toURL, storePassword, keyPassword)

  def fromConfig(config: Config, default: Path): Checked[KeyStoreRef] =
    config.checkedPath("js7.web.https.keystore.store-password")(path =>
      Right(
        KeyStoreRef(
          config.as[Path]("js7.web.https.keystore.file", default).toAbsolutePath,
          storePassword = config.as[SecretString](path),
          keyPassword = config.as[SecretString]("js7.web.https.keystore.key-password"))))
}
