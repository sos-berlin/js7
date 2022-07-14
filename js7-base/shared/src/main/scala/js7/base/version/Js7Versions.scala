package js7.base.version

import js7.base.Js7Version
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*

object Js7Versions
{
  def checkNonMatchingVersion(
    otherVersion: Version,
    otherName: => String,
    ourVersion: Version = Js7Version)
  : Checked[Unit] =
    ourVersion.isMajorMinorEqual(otherVersion) !!
      Problem(s"$otherName version $otherVersion does not match our version $ourVersion")
}
