package js7.license

/** ServiceProvider interface for license checking. */
trait LicenseCheck
{
  def hasLicense(productName: String): Boolean
}
