package js7.base.utils

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenItTakesLonger}
import scala.concurrent.duration.Duration

object AllocatedForJvm:

  extension[F[_], A](allocated: Allocated[F, A])
    def useSync[R](stopTimeout: Duration)(body: A => R)(using IORuntime, sourcecode.Enclosing): R =
      val stop = allocated.release.asInstanceOf[IO[Unit]]
      val ac: AutoCloseable = () =>
        stop
          .logWhenItTakesLonger(s"${allocated.toAllocatedString}.useSync.stop")
          .await(stopTimeout)

      autoClosing(ac)(_ => body(allocated.allocatedThing))


  extension[A](resource: ResourceIO[A])
    def useSync[R](timeout: Duration)(body: A => R)
      (using Tag[A], IORuntime, sourcecode.Enclosing)
    : R =
      resource.toAllocated.await(timeout).useSync(timeout)(body)
