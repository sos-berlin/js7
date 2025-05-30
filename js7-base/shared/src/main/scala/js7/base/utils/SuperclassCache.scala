package js7.base.utils

import js7.base.utils.Classes.superclassesOf

final class SuperclassCache[U](upperClass: Class[U]):

  private val memoizer = Memoizer.nonStrict1[Class[?], Set[Class[?]]](
    cls => superclassesOf[U, U](cls.asInstanceOf[Class[U]], upperClass).asInstanceOf[Set[Class[?]]]
  )(using showMemoizer)

  /** Returns a Set of class, and all superclasses and interfaces below `U`. */
  def assignableClasses[A <: U](cls: Class[A]): Set[Class[? >: A <: U]] =
    memoizer(cls).asInstanceOf[Set[Class[? >: A <: U]]]

  override def toString =
    memoizer.toString

  private def showMemoizer: Memoizer.Show[Class[?], Set[Class[?]]] =
    memoizer =>
      val map = memoizer.toMap
      s"SuperclassCache(${map.size} classes) ⏎" +
        map
          .map { case (k, v) =>
            s"\n  ${k.getName} ->" +
              v.toVector.sortBy(_.getName).map("\n    " + _.getName).mkString
          }
          .mkString
