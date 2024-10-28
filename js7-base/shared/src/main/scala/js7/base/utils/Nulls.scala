package js7.base.utils

import js7.base.utils.Nulls.*

object Nulls:

  /** Like Option apply, but compatible with -Yexplicit-nulls. */
  def nullToNone[A](a: A | Null): Option[A] =
    a match
      case null => None
      case a: A @unchecked => Some(a)

  /** In case A may be a disguised null. */
  inline def isNull[A](a: A): Boolean =
    (a: A | Null) == null

  /** For A which may be a disguised null. */
  inline def nonNull[A](a: A): Boolean =
    !isNull(a)

  extension [A](a: A | Null)
    inline def notNullOr[B >: A](whenNull: => B): B =
      a match
        case null => whenNull
        case a: A @unchecked => a


object NonNull:
  inline def unapply[A](a: A | Null): Option[A] =
    nullToNone(a)


//object IsNull:
//  private val someUnit = Some(())
//
//  DOES NOT WORK
//  inline def unapply[A](a: A): Option[Unit] =
//    if a == null then someUnit else None
