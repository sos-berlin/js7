package js7.licensefake

import js7.license.LicenseCheck

final class FakeLicenseCheck extends LicenseCheck
{
  def hasLicense(productName: String) = true
}
