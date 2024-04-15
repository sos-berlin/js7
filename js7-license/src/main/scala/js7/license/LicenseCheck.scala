package js7.license

/** ServiceProvider interface for license checking. */
trait LicenseCheck:
  /** Called once after construction and before `hasLincense`. */
  def initialize(context: LicenseCheckContext): Unit = {}

  def hasLicense(productName: String): Boolean
