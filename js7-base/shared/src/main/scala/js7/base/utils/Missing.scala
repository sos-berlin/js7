package js7.base.utils

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

    def foreach(body: A => Unit): Unit =
      underlying match
        case Missing =>
        case a: A @unchecked => body(a)
