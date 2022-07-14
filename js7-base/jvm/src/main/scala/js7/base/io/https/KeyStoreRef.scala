package js7.base.io.https

import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.problem.Checked

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

  def clientFromConfig(config: Config, configDirectory: Path): Checked[KeyStoreRef] =
    if (config.hasPath("js7.web.https.client-keystore"))
      fromSubconfig(config.getConfig("js7.web.https.client-keystore"),
        defaultFile = configDirectory.resolve("private/https-client-keystore.p12"))
    else
      config.checkedPath("js7.web.https.keystore")(path =>
        fromSubconfig(config.getConfig(path),
          defaultFile = configDirectory.resolve("private/https-keystore.p12")))

  def fromSubconfig(subconfig: Config, defaultFile: Path): Checked[KeyStoreRef] =
    subconfig.checkedPath("store-password")(path =>
      Right(
        KeyStoreRef(
          subconfig.as[Path]("file", defaultFile).toAbsolutePath,
          storePassword = subconfig.as[SecretString](path),
          keyPassword = subconfig.as[SecretString]("key-password"))))
}
