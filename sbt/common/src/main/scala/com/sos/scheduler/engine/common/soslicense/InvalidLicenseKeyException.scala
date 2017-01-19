package com.sos.scheduler.engine.common.soslicense

/**
 * @author Joacim Zschimmer
 */
final class InvalidLicenseKeyException(val key: LicenseKeyString) extends RuntimeException {
  override def getMessage = s"Invalid license key: $key"
}
