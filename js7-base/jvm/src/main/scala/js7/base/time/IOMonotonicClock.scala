//package js7.base.time
//
//import cats.effect.IO
//import cats.effect.unsafe.IORuntime
//
//final class IOMonotonicClock()(using runtime: IORuntime) extends MonotonicClock:
//  def now = deadline(IO.monotonic.unsafeRunSync())
