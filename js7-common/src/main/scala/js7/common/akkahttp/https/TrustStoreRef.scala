package js7.common.akkahttp.https

import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.common.configutils.Configs._

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
  def fromConfig(config: Config, default: Path): Checked[TrustStoreRef] =
    config.checkedPath("js7.https.truststore.store-password")(path =>
      Right(
        TrustStoreRef(
          url = config.as[Path]("js7.https.truststore.file", default).toAbsolutePath.toUri.toURL,
          storePassword = config.as[SecretString](path))))
}
