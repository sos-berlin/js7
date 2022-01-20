package js7.base.version

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.version.Version._
import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

final case class Version private(
  string: String,
  major: Int, minor: Int, patch: Int,
  prerelease: List[String] = Nil,
  build: List[String] = Nil)
extends Ordered[Version]
{
  def compare(o: Version) =
    if (this eq o)
      0
    else
      (major, minor, patch) compare (o.major, o.minor, o.patch) match {
        case 0 =>
          (prerelease, o.prerelease) match {
            case (_ :: _, Nil) => -1
            case (Nil, _ :: _) => +1
            case (a, b) => compareLists(a, b)
          }
        case n => n
      }

  override def toString = string
}

object Version
{
  private val VersionRegex =
    """([0-9]+)\.([0-9]+)\.([0-9]+)(-([A-Za-z0-9.]+))?(\+([A-Za-z0-9]+))?""".r

  def apply(string: String): Version =
    checked(string).orThrow

  def checked(string: String): Checked[Version] =
    string match {
      case VersionRegex(major, minor, patch, _, prereleaseGroup, _, buildGroup) =>
        val prerelease = Option(prereleaseGroup).fold[List[String]](Nil)(_.split("\\.").toList)
        val build = Option(buildGroup).fold[List[String]](Nil)(_.split("\\.").toList)
        Checked.catchNonFatal(
          new Version(string, major.toInt, minor.toInt, patch.toInt, prerelease, build))

      case _ =>
        Left(Problem(s"Unrecognized version: $string"))
    }

  @tailrec private def compareLists(a: List[String], b: List[String]): Int =
    (a, b) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (a :: aa, b :: bb) => a.compare(b) match {
        case 0 => compareLists(aa, bb)
        case n => n
      }
    }
}
