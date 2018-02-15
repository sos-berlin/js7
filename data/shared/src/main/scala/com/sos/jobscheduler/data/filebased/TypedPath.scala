package com.sos.jobscheduler.data.filebased

import cats.syntax.flatMap._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Checked.ops.RichOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.filebased.TypedPath._
import java.nio.file.{Path, Paths}
import scala.reflect.ClassTag

trait TypedPath
extends AbsolutePath {

  def companion: Companion[_ <: TypedPath]

  def file(t: SourceType): Path =
    Paths.get(withoutStartingSlash + companion.sourceTypeToFilenameExtension(t))

  def asTyped[A <: TypedPath: TypedPath.Companion]: A = {
    val c = implicitly[TypedPath.Companion[A]]
    if (c == companion)
      this.asInstanceOf[A]
    else
      c.apply(string)
  }

  override def toString = toTypedString

  def pretty: String = s"${companion.camelName} $string"

  def toTypedString: String = s"${companion.camelName}:$string"
}

object TypedPath {
  implicit def ordering[P <: TypedPath]: Ordering[P] =
    (a, b) ⇒ a.string compare b.string

  type AnyCompanion = Companion[_ <: TypedPath]

  abstract class Companion[P <: TypedPath: ClassTag] extends AbsolutePath.Companion[P]
  {
    def sourceTypeToFilenameExtension: Map[SourceType, String]

    implicit val implicitCompanion: Companion[P] = this
    final val camelName: String = name stripSuffix "Path"

    final def typedPathClass: Class[P] = implicitClass[P]

    def fromFile(string: String): Checked[(P, SourceType)] =
      sourceTypeToFilenameExtension.collectFirst { case (t, ext) if string endsWith ext ⇒
        Checked.catchNonFatal(apply(string dropRight ext.length) → t)
      }
      .toChecked(Problem(s"Not a $name: $string"))
      .flatten

    /**
     * Interprets a path as absolute.
     *
     * @param path A string starting with "./" is rejected
     */
    final def makeAbsolute(path: String): P =
      apply(absoluteString(path))
  }

  /**
   * Interprets a path as absolute.
   *
   * @param path A string starting with "./" is rejected
   */
  private def absoluteString(path: String): String =
    if (AbsolutePath.isAbsolute(path))
      path
    else {
      require(!(path startsWith "./"), s"Relative path is not possible here: $path")
      s"/$path"
    }
}
