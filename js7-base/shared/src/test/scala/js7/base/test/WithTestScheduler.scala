package js7.base.test

import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import js7.base.time.TestScheduler
import scala.concurrent.ExecutionContext

trait WithTestScheduler:
  protected final val scheduler: TestScheduler = TestScheduler()

  protected final def ioRuntime: IORuntime =
    _ioRuntime

  private lazy val _ioRuntime: IORuntime =
    IORuntime(
      compute = ExecutionContext.global,
      blocking = ExecutionContext.global/*???*/,
      scheduler,
      shutdown = () => {},
      IORuntimeConfig())
