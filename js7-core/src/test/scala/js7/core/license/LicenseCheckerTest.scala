package js7.core.license

import js7.core.license.LicenseChecker.hasLicense
import org.scalatest.freespec.AnyFreeSpec

final class LicenseCheckerTest extends AnyFreeSpec
{
  private case class TestLicenseCheck(value: Boolean) extends LicenseCheck {
    def hasLicense(productName: String) = value
  }

  "hasLicense" in {
    assert(!hasLicense(Nil, ""))
    assert(!hasLicense(Seq(TestLicenseCheck(false)), ""))
    assert(!hasLicense(Seq(TestLicenseCheck(false), TestLicenseCheck(false)), ""))
    assert(hasLicense(Seq(TestLicenseCheck(true)), ""))
    assert(hasLicense(Seq(TestLicenseCheck(true), TestLicenseCheck(false)), ""))
    assert(hasLicense(Seq(TestLicenseCheck(false), TestLicenseCheck(true)), ""))
  }
}
