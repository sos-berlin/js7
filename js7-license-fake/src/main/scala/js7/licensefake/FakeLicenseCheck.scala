package js7.licensefake

import java.nio.file.Files.isDirectory
import js7.license.{LicenseCheck, LicenseCheckContext}

final class FakeLicenseCheck extends LicenseCheck:
  private var context: LicenseCheckContext | Null = null

  override def initialize(context: LicenseCheckContext): Unit =
    this.context = context

  def hasLicense(productName: String): Boolean =
    if !isDirectory(context.nn.configDirectory) then throw new AssertionError // will never happen
    true

  override def toString = "FakeLicenseCheck"
