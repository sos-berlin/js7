package js7.base.test

import cats.effect.unsafe.IORuntime
import scala.concurrent.ExecutionContext

trait TestCatsEffect:
  //this: OurAsyncTestSuite =>

  // Or use own IORuntime for each test class ???
  protected implicit def ioRuntime: IORuntime =
    IORuntime.global

  implicit def executionContext: ExecutionContext =
    IORuntime.global.compute
