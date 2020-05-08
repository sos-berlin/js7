package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.configutils.Configs._
import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path

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
    config.checkedPath("jobscheduler.https.truststore.store-password")(path =>
      Right(
        TrustStoreRef(
          url = config.as[Path]("jobscheduler.https.truststore.file", default).toAbsolutePath.toUri.toURL,
          storePassword = config.as[SecretString](path))))
}
