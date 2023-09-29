package js7.base.utils

object Classes
{
  /** Returns a Set of class, and all superclasses and interfaces below `A`. */
  def superclassesOf[A](cls: Class[A]): Set[Class[? >: A]] =
    superclassesOf[A, Any](cls, classOf[Any])

  def superclassesOf[A <: U, U](cls: Class[A], upperClass: Class[U]): Set[Class[? >: A <: U]] =
    superOf(cls, upperClass).asInstanceOf[Set[Class[? >: A <: U]]]

  private def superOf[U](cls: Class[?], upperClass: Class[U]): Set[Class[? <: U]] =
    if cls == null || !upperClass.isAssignableFrom(cls) then
      Set.empty
    else
      (cls.getInterfaces :+ cls.getSuperclass)
        .flatMap(superOf(_, upperClass))
        .toSet ++
        Set(cls.asInstanceOf[Class[? <: U]])
}
