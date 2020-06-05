package js7.base.eventbus

/**
  * @author Joacim Zschimmer
  */
final class SuperclassCache[A](upperSuperclass: Class[A])
{
  private val store = new java.util.concurrent.ConcurrentHashMap[Class[_ <: A], Set[Class[_ <: A]]]

  def contains[A1 <: A](cls: Class[A1]) =
    store.contains(cls)

  /** Returns a Set of class, and all superclasses and interfaces below `A`. */
  def assignableClasses[A1 <: A](clas: Class[A1]): Set[Class[_ >: A1 <: A]] =
    (store.get(clas) match {
      case null =>
        val classes = superOf(clas)
        store.put(clas, classes)
        classes
      case o => o
    }).asInstanceOf[Set[Class[_ >: A1 <: A]]]

  private def superOf(clas: Class[_]): Set[Class[_ <: A]] =
    if (clas == null || !upperSuperclass.isAssignableFrom(clas))
      Set.empty
    else
      (clas.getInterfaces :+ clas.getSuperclass)
        .flatMap(superOf).toSet ++ Set(clas.asInstanceOf[Class[_ <: A]])
}
