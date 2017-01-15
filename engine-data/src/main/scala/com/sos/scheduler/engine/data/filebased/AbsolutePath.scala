package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.data.filebased.AbsolutePath._

trait AbsolutePath extends IsString {

  def companion: Companion[_ <: AbsolutePath]

  lazy val name: String = string.substring(string.lastIndexOf('/') + 1)

  lazy val nesting = string stripSuffix "/" count { _ == '/' }

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** Has to be called in every implementing constructor. */
  protected def validate(): Unit = {
    require(companion.isEmptyAllowed || string.nonEmpty, s"Name '$name' must not be the empty string in $errorString")
    require(string.isEmpty || string.startsWith("/"), s"Absolute path expected in $errorString")
    if (!companion.isSingleSlashAllowed || string != "/") {
      require(!string.endsWith("/"), s"Trailing slash not allowed in $errorString")
    }
    require(!string.contains("//"), s"Double slash not allowed in $errorString")
    require(companion.isCommaAllowed || !string.contains(","), s"Comma not allowed in $errorString")
  }

  private def errorString = s"${companion.name} '$string'"
}

object AbsolutePath {

  def isAbsolute(path: String): Boolean =
    path startsWith "/"

  trait Companion[A <: AbsolutePath] extends IsString.Companion[A] {

    val name = getClass.getSimpleName stripSuffix "$"
    val NameOrdering: Ordering[A] = Ordering by { _.name }

    def apply(o: String): A

    protected[engine] def isEmptyAllowed = false
    protected[engine] def isSingleSlashAllowed = false
    protected[engine] def isCommaAllowed = true
  }

  private[data] final case class Untyped(string: String) extends AbsolutePath {
    validate()
    def companion = Untyped
  }

  private[data] object Untyped extends Companion[Untyped]
}
