package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.configutils.Configs._
import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path

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
  def fromConfig(config: Config, default: Path): Checked[KeyStoreRef] =
    config.checkedPath("jobscheduler.https.keystore.store-password")(path =>
      Right(
        KeyStoreRef(
          url = config.as[Path]("jobscheduler.https.keystore.file", default).toAbsolutePath.toUri.toURL,
          storePassword = config.as[SecretString](path),
          keyPassword = config.as[SecretString]("jobscheduler.https.keystore.key-password"))))
}
