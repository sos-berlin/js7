package js7.base.utils

import js7.base.problem.Checked
import scala.reflect.ClassTag

/** Map for subclasses to X, derived from the given superclass Map. */
final class SubclassToX[C, X](superclassToX: Map[Class[? <: C], X])(implicit C: ClassTag[C])
{
  private val toSuperclass = new ToSuperclass[C](superclassToX.keySet)

  def checked[C1 <: C](cls: Class[C1]): Checked[X] =
    toSuperclass.checked(cls).map(superclassToX)

  def get[C1 <: C](cls: Class[C1]): Option[X] =
    toSuperclass.get(cls).map(superclassToX)
}
