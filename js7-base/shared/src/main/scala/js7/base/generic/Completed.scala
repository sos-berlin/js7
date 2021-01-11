package js7.base.generic

import cats.Monoid

/**
  * May be used for Future[Completed].
  * Like Akka's `Done`.
  *
  * @author Joacim Zschimmer
  */
object Completed
{
  // type Completed = Completed.type    <-- defined in package.scala

  override def toString = "Completed"

  implicit val CompletedMonoid: Monoid[Completed] =
    new Monoid[Completed]
    {
      val empty = Completed

      def combine(a: Completed, b: Completed) =
        Completed

      override def combineN(completed: Completed, n: Int) =
        Completed

      override def combineAll(iterable: IterableOnce[Completed]) =
        Completed

      override def reverse: Monoid[Completed] =
        this
    }

  val combine = CompletedMonoid.combine _
}
