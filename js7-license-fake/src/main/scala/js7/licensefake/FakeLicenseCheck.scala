package js7.licensefake

import java.nio.file.Files.isDirectory
import js7.license.{LicenseCheck, LicenseCheckContext}

final class FakeLicenseCheck extends LicenseCheck {
  private var context: LicenseCheckContext = null

  override def initialize(context: LicenseCheckContext) =
    this.context = context

  def hasLicense(productName: String) = {
    if (!isDirectory(context.configDirectory)) throw new AssertionError // will never happen
    true
  }

  override def toString = "FakeLicenseCheck"
}
