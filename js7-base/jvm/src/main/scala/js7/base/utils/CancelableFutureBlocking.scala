package js7.base.utils

import js7.base.thread.Futures.implicits.SuccessFuture

object CancelableFutureBlocking:

  extension [A](cancelableFuture: CancelableFuture[A])

    def blockingCancel()(using sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line): Unit =
      cancelableFuture.cancelToFuture().awaitInfinite
