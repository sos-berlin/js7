package js7.base.generic

/**
  * Accepted, but maybe not yet `Completed`, may be used for Task[Accepted].
  * Like Akka's `Done`.
  *
  * @author Joacim Zschimmer
  */
sealed trait Accepted

/**
  * May be used for Task[Accepted].
  * Like Akka's `Done`.
  */
object Accepted extends Accepted:
  override def toString = "Accepted"
