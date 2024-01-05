package js7.base.test

import cats.effect.unsafe.IORuntime
import js7.base.catsutils.OurIORuntime
import scala.concurrent.ExecutionContext

trait TestCatsEffect:

  // Or use own IORuntime for each test class ???
  protected implicit final def ioRuntime: IORuntime =
    //IORuntime.global
    OurIORuntime.ioRuntime

  implicit def executionContext: ExecutionContext =
    ioRuntime.compute
