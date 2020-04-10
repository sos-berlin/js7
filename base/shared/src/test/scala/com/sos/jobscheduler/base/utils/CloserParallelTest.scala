package com.sos.jobscheduler.base.utils

import scala.concurrent.Future
import org.scalatest.freespec.AsyncFreeSpec

final class CloserParallelTest extends AsyncFreeSpec
{
  "Parallel execution" in {
    val closer = new Closer
    val ctx = new CloserTest.Context
    val closeables = Vector.fill(100000) { new ctx.TestCloseable }
    for (c <- closeables) closer.register(c)
    Future.sequence((1 to sys.runtime.availableProcessors).map(_ => Future(closer.close())))
      .map { _ =>
        assert(closeables forall (_.isClosed))
        assert(ctx.closed.toSet == closeables.toSet)
      }
  }
}
