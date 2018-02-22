package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.problem.Checked.ops.RichChecked
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.filebased.AbsolutePath._

trait AbsolutePath extends IsString {

  def companion: Companion[_ <: AbsolutePath]

  lazy val name: String = string.substring(string.lastIndexOf('/') + 1)

  def nesting = string stripSuffix "/" count { _ == '/' }

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** Has to be called in every implementing constructor. */
  final def validate() = companion.check(string).force
}

object AbsolutePath {

  def isAbsolute(path: String): Boolean =
    path startsWith "/"

  trait Companion[A <: AbsolutePath] extends IsString.Companion[A] {

    val name = getClass.getSimpleName stripSuffix "$"
    val NameOrdering: Ordering[A] = Ordering by { _.name }

    def apply(o: String): A

    def isEmptyAllowed = false
    def isSingleSlashAllowed = false
    def isCommaAllowed = true

    final def checked(string: String): Checked[A] =
      check(string) map (_ ⇒ apply(string))

    private[AbsolutePath] def check(string: String): Checked[Unit] = {
      def errorString = s"$name '$string'"
      if (!isEmptyAllowed && string.isEmpty)
        Problem(s"Must not be the empty string in $errorString")
      else if (string.nonEmpty && !string.startsWith("/"))
        Problem(s"Absolute path expected in $errorString")
      else if (string.endsWith("/") && (!isSingleSlashAllowed || string != "/"))
        Problem(s"Trailing slash not allowed in $errorString")
      else if (string contains "//")
        Problem(s"Double slash not allowed in $errorString")
        else if (!isCommaAllowed && string.contains(","))
        Problem(s"Comma not allowed in $errorString")
      else
        Checked(())
    }
  }

  private[data] final case class Untyped(string: String) extends AbsolutePath {
    validate()
    def companion = Untyped
  }

  private[data] object Untyped extends Companion[Untyped]
}
