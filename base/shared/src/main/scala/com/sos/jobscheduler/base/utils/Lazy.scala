package js7.base.utils

/**
  * Like Scala lazy but synchronization moved to own object.
  *
  * @author Joacim Zschimmer
  */
import scala.language.implicitConversions

final class Lazy[A] private(eval: => A)
{
  @volatile
  private var cache: Option[A] = None

  def apply(): A =
    cache match {
      case Some(a) => a
      case None =>
        synchronized {
          cache match {
            case Some(a) => a
            case None =>
              val a = eval
              cache = Some(a)
              a
          }
        }
    }

  def toOption: Option[A] = cache

  def isDefined = cache.isDefined

  def isEmpty = cache.isEmpty

  def foreach(f: A => Unit): Unit =
    cache foreach f

  /** To allow `for (x <- myLazy if predicate(x)) {...}` .*/
  def withFilter(predicate: A => Boolean): Option[A]#WithFilter =
    toOption withFilter predicate
}

object Lazy
{
  def apply[A](f: => A): Lazy[A] =
    new Lazy(f)

  implicit def evalLazy[A](o: Lazy[A]): A =
    o()
}
