package js7.common.soslicense

import js7.common.soslicense.LicenseKeyString._
import scala.PartialFunction.cond

/**
 * @author Joacim Zschimmer
 */
final class LicenseKeyString(val string: String) {
  if (string contains ' ') throw new IllegalArgumentException("Multiple license keys are not supported")  // Use Iterable[LicenseKeyString]

  val normalizedString = normalize(string)

  override def hashCode = normalizedString.hashCode

  override def equals(o: Any) = cond(o) { case o: LicenseKeyString => normalizedString == o.normalizedString }

  override def toString = string
}

object LicenseKeyString {
  def apply(key: String) = new LicenseKeyString(key.trim)

  def normalize(o: String) = o.toUpperCase
}
