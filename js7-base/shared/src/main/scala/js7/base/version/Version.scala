package js7.base.version

import js7.base.generic.GenericString
import js7.base.problem.Checked.catchExpected
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.Version.compareLists
import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

/** Semantic version. */
final case class Version(
  string: String,
  major: Int,
  minor: Int,
  patch: Int,
  prerelease: List[String] = Nil,
  build: List[String] = Nil)
extends GenericString with Ordered[Version]:
  def compare(o: Version) =
    if this eq o then
      0
    else
      (major, minor, patch) compare (o.major, o.minor, o.patch) match
        case 0 =>
          (prerelease, o.prerelease) match
            case (_ :: _, Nil) => -1
            case (Nil, _ :: _) => +1
            case (a, b) => compareLists(a, b)
        case n => n

  def isMajorMinorEqual(other: Version): Boolean =
    major == other.major && minor == other.minor

  override def toString = string

object Version extends GenericString.Checked_[Version]:
  private val VersionRegex =
  """([0-9]+)\.([0-9]+)\.([0-9]+)(-([A-Za-z0-9.]+))?(\+([A-Za-z0-9]+))?""".r

  protected def unchecked(string: String) =
    checked(string).orThrow

  override def checked(string: String): Checked[Version] =
    string match
      case VersionRegex(major, minor, patch, _, prereleaseGroup, _, buildGroup) =>
        val prerelease = Option(prereleaseGroup).fold[List[String]](Nil)(_.split("\\.").toList)
        val build = Option(buildGroup).fold[List[String]](Nil)(_.split("\\.").toList)
        catchExpected[NumberFormatException](
          new Version(string, major.toInt, minor.toInt, patch.toInt, prerelease, build))

      case _ =>
        Left(Problem(s"Unrecognized version: $string"))

  @tailrec private def compareLists(a: List[String], b: List[String]): Int =
    (a, b) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (a :: aa, b :: bb) => a.compare(b) match
        case 0 => compareLists(aa, bb)
        case n => n
