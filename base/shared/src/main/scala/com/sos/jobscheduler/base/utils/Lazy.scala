package com.sos.jobscheduler.base.utils

/**
  * Like Scala lazy but synchronization moved to own object.
  *
  * @author Joacim Zschimmer
  */
import scala.language.implicitConversions

final class Lazy[A] private(eval: => A)
{
  @volatile
  private var option: Option[A] = None

  def apply(): A =
    option match {
      case Some(a) => a
      case None =>
        synchronized {
          option match {
            case Some(a) => a
            case None =>
              val a = eval
              option = Some(a)
              a
          }
        }
    }

  def toOption: Option[A] = option
}

object Lazy
{
  def apply[A](f: => A): Lazy[A] =
    new Lazy(f)

  implicit def evalLazy[A](o: Lazy[A]): A =
    o()

  implicit def lazyToOption[A](o: Lazy[A]): Option[A] =
    o.toOption
}
