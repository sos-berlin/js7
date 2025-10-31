package js7.core.license

import java.nio.file.Path
import js7.base.test.OurTestSuite
import js7.license.{LicenseCheck, LicenseCheckContext}

final class LicenseCheckerTest extends OurTestSuite:
  private val licenseCheckContext = LicenseCheckContext(Path.of("/CONFIG"))

  private case class TestLicenseCheck(value: Boolean) extends LicenseCheck:
    def hasLicense(productName: String) = value

  "hasLicense" in:
    val licenseChecker = new LicenseChecker(licenseCheckContext)
    assert(!licenseChecker.hasLicense(Nil, "PRODUCT"))
    assert(!licenseChecker.hasLicense(Seq(TestLicenseCheck(false)), "PRODUCT"))
    assert(!licenseChecker.hasLicense(Seq(TestLicenseCheck(false), TestLicenseCheck(false)), "PRODUCT"))
    assert(licenseChecker.hasLicense(Seq(TestLicenseCheck(true)), "PRODUCT"))
    assert(licenseChecker.hasLicense(Seq(TestLicenseCheck(true), TestLicenseCheck(false)), "PRODUCT"))
    assert(licenseChecker.hasLicense(Seq(TestLicenseCheck(false), TestLicenseCheck(true)), "PRODUCT"))
