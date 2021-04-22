package com.js7.licensedummy

import js7.core.license.LicenseCheck

final class DummyLicenseCheck extends LicenseCheck
{
  def hasLicense(productName: String) = true
}
