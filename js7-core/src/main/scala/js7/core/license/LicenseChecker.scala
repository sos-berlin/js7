package js7.core.license

import java.util.ServiceLoader
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.RightUnit
import scala.jdk.CollectionConverters._

object LicenseChecker
{
  private val logger = Logger[this.type]

  private lazy val licenseChecks: Seq[LicenseCheck] = {
    val serviceLoader = ServiceLoader.load(classOf[LicenseCheck])
    val iterator = serviceLoader.iterator.asScala
    if (iterator.isEmpty) logger.debug("No LicenseCheck")
    serviceLoader.asScala.toSeq
  }

  def checkLicense(productName: String): Checked[Unit] =
    if (!hasLicense(licenseChecks, productName))
      Left(Problem(s"No license for $productName"))
    else
      RightUnit

  private[license] def hasLicense(licenseChecks: Seq[LicenseCheck], productName: String): Boolean =
    licenseChecks
      .view
      .dropWhile(o => !o.hasLicense(productName))
      .nonEmpty
}
