package js7.base.utils

import cats.Applicative

/** Use with care.
  *
  * Use Missing only for optional parameters.
  *
  * Because Missing is an optional type, this operations are compilable for types without Missing !!!
  * So use them with care and be sure to use them only for sum types with Missing,
  * otherwise it would confuse the reader.
  *
  * --> Could a macro via type inspection require the Missing type?
  */
type Missing = Missing.type

/** Marker for a missing value, like Option None. */
object Missing:

  extension [A](underlying: A | Missing)

    def toOption: Option[A] =
      underlying match
        case Missing => None
        case a: A @unchecked => Some(a)

    infix def getOrElse[B >: A](b: B): B =
      underlying match
        case Missing => b
        case a: A @unchecked => a

    def map[B](f: A => B): B | Missing =
      underlying match
        case Missing => Missing
        case a: A @unchecked => f(a)

    def foldMap[F[_]: Applicative as F, B](f: A => F[B | Missing]): F[B | Missing] =
      underlying match
        case Missing => F.pure(Missing)
        case a: A @unchecked => f(a)

    def foreach(body: A => Unit): Unit =
      underlying match
        case Missing =>
        case a: A @unchecked => body(a)

    def notMissingOrThrow: A =
      underlying match
        case Missing => throw new NoSuchElementException("Missing")
        case a: A @unchecked => a

    def notMissingOrThrow(name: String): A =
      underlying match
        case Missing => throw new NoSuchElementException(s"$name is Missing")
        case a: A @unchecked => a
