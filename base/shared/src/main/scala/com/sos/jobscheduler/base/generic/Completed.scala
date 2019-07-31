package com.sos.jobscheduler.base.generic

import cats.Monoid

/**
  * May be used for Future[Completed].
  * Like Akka's `Done`.
  *
  * @author Joacim Zschimmer
  */
object Completed
{
  // type Completed = Completed.type    <-- define in package.scala

  override def toString = "Completed"

  implicit val CompletedMonoid: Monoid[Completed.type] = new Monoid[Completed.type]
  {
    val empty = Completed

    def combine(a: Completed.type, b: Completed.type) = Completed
  }

  val combine = CompletedMonoid.combine _
}
