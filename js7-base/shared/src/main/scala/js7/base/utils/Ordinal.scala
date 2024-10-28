package js7.base.utils

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

// Typeclass
@implicitNotFound(msg = "No implicit Ordinal[${A}] defined.")
trait Ordinal[A]:

  // succ, pred: throw on overflow ???

  /** Successor. */
  def succ(a: A): A

  /** Predeccessor. */
  def pred(a: A): A

  /** a isSuccessorOf b. */
  def isSuccessorOf(a: A, b: A): Boolean =
    a == succ(b)


object Ordinal:
  implicit def apply[A](implicit ordinal: Ordinal[A]): Ordinal[A] =
    ordinal

  implicit val IntOrdinal: Ordinal[Int] =
    new Ordinal[Int]:
      def succ(a: Int) = a + 1

      def pred(a: Int) = a - 1

      override def isSuccessorOf(a: Int, b: Int) =
        a != Int.MaxValue && a == b + 1

  implicit val CharOrdinal: Ordinal[Char] =
    new Ordinal[Char]:
      def succ(a: Char) = (a + 1).toChar

      def pred(a: Char) = (a - 1).toChar

      // Char is treated as an unsigned value, despite Scala looks at it as a signed value.
      override def isSuccessorOf(a: Char, b: Char) =
        a != '\uFFFF' && a == b + 1

  trait Ops[A]:
    infix def isSuccessorOf(a: A): Boolean
    def succ: A
    def pred: A

  object syntax:
    implicit def toOps[A](a: A)(implicit ordinal: Ordinal[A]): Ops[A] =
      new Ops[A]:
        def succ = ordinal.succ(a)
        def pred = ordinal.pred(a)
        override def isSuccessorOf(b: A) = ordinal.isSuccessorOf(a, b)
