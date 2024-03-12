package js7.base.utils

type UseDefault = UseDefault.type

/** Marker for a default value, like Option None. */
object UseDefault:

  extension [A](underlying: A | UseDefault)

    def toOption: Option[A] =
      underlying match
        case UseDefault => None
        case a: A @unchecked => Some(a)

    def getOrElse[B >: A](b: B): B =
      underlying match
        case UseDefault => b
        case a: A @unchecked => a
