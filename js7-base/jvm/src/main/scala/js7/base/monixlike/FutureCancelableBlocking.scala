package js7.base.monixlike

import js7.base.scalasource.ScalaSourceLocation
import js7.base.thread.Futures.implicits.SuccessFuture

object FutureCancelableBlocking:

  extension (cancelable: FutureCancelable)
    def blockingCancel()(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
      cancelable match
        case o: StandardFutureCancelable => o.cancelToFuture().awaitInfinite
        case EmptyFutureCancelable =>
