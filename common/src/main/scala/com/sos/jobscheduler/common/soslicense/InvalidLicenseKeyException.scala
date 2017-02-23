package com.sos.jobscheduler.common.soslicense

/**
 * @author Joacim Zschimmer
 */
final class InvalidLicenseKeyException(val key: LicenseKeyString) extends RuntimeException {
  override def getMessage = s"Invalid license key: $key"
}
