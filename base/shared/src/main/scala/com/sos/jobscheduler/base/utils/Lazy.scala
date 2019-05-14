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

  //def map[B](f: A => B): Option[B] =
  //  cache map f

  def foreach(f: A => Unit): Unit =
    cache foreach f
}

object Lazy
{
  def apply[A](f: => A): Lazy[A] =
    new Lazy(f)

  implicit def evalLazy[A](o: Lazy[A]): A =
    o()
}
