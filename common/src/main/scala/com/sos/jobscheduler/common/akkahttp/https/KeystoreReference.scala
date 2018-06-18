package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.typesafe.config.Config
import java.net.URL
import java.nio.file.{Files, Path}

/**
  * @author Joacim Zschimmer
  */
final case class KeystoreReference(
  url: URL,
  /** Password for .jks file, just to check integrity (may be empty) */
  storePassword: Option[SecretString] = None,
  /** PKCS#12 key password */
  keyPassword: Option[SecretString] = None)
{
  override def toString: String = s"KeystoreReference($url)"
}

object KeystoreReference {
  private val JksPath = "private/private-https.jks"

  def fromConfig(config: Config, configDirectory: Path): Checked[KeystoreReference] = {
    val checkedFile = config.checkedAs[Path]("jobscheduler.webserver.https.keystore.file") findValid {
      val file = configDirectory resolve JksPath
      (Files.exists(file) ? file) toChecked Problem(s"For HTTPS, a keystore file '$file' is needed")
    }
    checkedFile
      .map (file ⇒ KeystoreReference(
        url = file.toAbsolutePath.toURI.toURL,
        storePassword = config.optionAs[SecretString]("jobscheduler.webserver.https.keystore.password"),
        keyPassword = Some(config.as[SecretString]("jobscheduler.webserver.https.keystore.key-password")))
      )}
}
