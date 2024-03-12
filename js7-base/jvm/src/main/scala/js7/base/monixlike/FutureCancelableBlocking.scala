package js7.base.monixlike

import js7.base.thread.Futures.implicits.SuccessFuture

object FutureCancelableBlocking:

  extension (cancelable: FutureCancelable)
    def blockingCancel()(using sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line): Unit =
      cancelable match
        case o: StandardFutureCancelable => o.cancelToFuture().awaitInfinite
        case o: SerialFutureCancelable => o.cancelToFuture().awaitInfinite
        case EmptyFutureCancelable =>
