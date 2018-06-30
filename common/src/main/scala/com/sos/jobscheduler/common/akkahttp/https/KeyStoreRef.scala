package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.typesafe.config.Config
import java.net.URL
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class KeyStoreRef(
  url: URL,
  /** Password for file */
  storePassword: SecretString,
  /** PKCS#12 key password */
  keyPassword: Option[SecretString] = None)
{
  override def toString = s"KeyStore $url"
}

object KeyStoreRef
{
  def fromConfig(config: Config, default: Path): Checked[KeyStoreRef] =
    for (storePassword ← Checked.catchNonFatal(config.as[SecretString]("jobscheduler.webserver.https.keystore.store-password"))) yield
      KeyStoreRef(
        url = config.as[Path]("jobscheduler.webserver.https.keystore.file", default).toAbsolutePath.toURI.toURL,
        storePassword = storePassword,
        keyPassword = config.optionAs[SecretString]("jobscheduler.webserver.https.keystore.key-password"))
}
