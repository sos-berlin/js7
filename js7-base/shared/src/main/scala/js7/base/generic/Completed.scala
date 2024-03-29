package js7.base.generic

import cats.kernel.CommutativeMonoid


type Completed = Completed.type

/**
  * May be used for IO[Completed].
  * Like Pekko's `Done`.
  *
  * @author Joacim Zschimmer
  */
object Completed:

  // type Completed = Completed.type    <-- defined in package.scala

  override def toString = "Completed"

  implicit val CompletedMonoid: CommutativeMonoid[Completed] =
    new CommutativeMonoid[Completed]:
      val empty = Completed

      def combine(a: Completed, b: Completed) =
        Completed

      override def combineN(completed: Completed, n: Int) =
        Completed

      override def combineAll(iterable: IterableOnce[Completed]) =
        Completed

      override def reverse: CommutativeMonoid[Completed] =
        this

  val combine: (Completed, Completed) => Completed =
    CompletedMonoid.combine
