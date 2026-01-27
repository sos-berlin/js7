package js7.base.utils

import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.*
import scala.reflect.ClassTag

/** Map for subclasses to X, derived from the given superclass Map. */
final class SubclassToX[C: ClassTag, X](superclassToX: Map[Class[? <: C], X]):
  private val toSuperclass = new ToSuperclass[C](superclassToX.keySet)

  def checked[C1 <: C](cls: Class[C1]): Checked[X] =
    toSuperclass.checked(cls).map(superclassToX)

  def get[C1 <: C](cls: Class[C1]): Option[X] =
    toSuperclass.get(cls).map(superclassToX)


object SubclassToX:

  def apply[C: ClassTag, X](superclassToX: Map[Class[? <: C], X])
  : SubclassToX[C, X] =
    new SubclassToX(superclassToX)

  def by[C: ClassTag, X](toClass: X => Class[? <: C], xs: X*)
  : SubclassToX[C, X] =
    new SubclassToX[C, X](xs.toKeyedMap(toClass))
