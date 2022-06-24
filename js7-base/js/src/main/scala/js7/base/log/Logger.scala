package js7.base.log

import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

object Logger
{
  def apply[A: ClassTag]: scribe.Logger =
    apply(implicitClass[A])

  def apply(c: Class[_]): scribe.Logger =
    scribe.Logger(normalizeClassName(c))

  def apply(name: String): scribe.Logger =
    scribe.Logger(name)

  /** Removes '$' from Scala's companion object class. */
  def normalizeClassName(c: Class[_]): String =
    c.getName stripSuffix "$"
}
