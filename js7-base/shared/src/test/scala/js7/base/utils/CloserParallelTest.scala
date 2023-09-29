package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import scala.concurrent.Future

final class CloserParallelTest extends OurAsyncTestSuite
{
  "Parallel execution" in {
    val closer = new Closer
    val ctx = new CloserTest.Context
    val closeables = Vector.fill(100000) { new ctx.TestCloseable }
    for c <- closeables do closer.register(c)
    Future.sequence((1 to sys.runtime.availableProcessors).map(_ => Future(closer.close())))
      .map { _ =>
        assert(closeables forall (_.isClosed))
        assert(ctx.closed.toSet == closeables.toSet)
      }
  }
}
