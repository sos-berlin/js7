package com.sos.scheduler.engine.common.sprayutils.https

import com.sos.scheduler.engine.base.generic.SecretString
import java.net.URL

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
