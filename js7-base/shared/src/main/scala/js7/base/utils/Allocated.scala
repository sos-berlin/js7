package js7.base.utils

import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.syntax.*

final class Allocated[F[_]: UnsafeMemoizable, +A](val allocatedThing: A, stop_ : F[Unit])
extends Stoppable[F]
{
  val stop = stop_.unsafeMemoize
  override def toString = allocatedThing.toString
}

object Allocated {
  def fromPair[F[_]: UnsafeMemoizable, A](pair: (A, F[Unit])): Allocated[F, A] =
    new Allocated[F, A](pair._1, pair._2)
}
