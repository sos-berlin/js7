package js7.base.utils

import cats.:<:
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import scala.concurrent.duration.Duration

object AllocatedForJvm:

  extension[F[_], A](allocated: Allocated[F, A])

    def useSync[R](stopTimeout: Duration)(body: A => R)
      (using IORuntime, F :<: IO, sourcecode.Enclosing)
    : R =
      val stop = allocated.release.asInstanceOf[IO[Unit]]
      val ac: AutoCloseable = () =>
        stop
          .logWhenItTakesLonger(s"${allocated.toAllocatedString}.useSync.stop")
          .await(stopTimeout)

      autoClosing(ac)(_ => body(allocated.allocatedThing))
