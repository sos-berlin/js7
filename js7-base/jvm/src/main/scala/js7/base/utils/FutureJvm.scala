package js7.base.utils

import java.util.concurrent.{CancellationException, CompletableFuture}
import java.util.function.BiFunction
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object FutureJvm:

  extension [A](future: Future[A])

    def asCompletableFuture(using ExecutionContext): CompletableFuture[A] =
      val javaFuture = new CompletableFuture[A]

      future.onComplete:
        case Success(a) => javaFuture.complete(a)
        case Failure(ex) => javaFuture.completeExceptionally(ex)

      future match
        case future: CancelableFuture[A] @unchecked =>
          javaFuture.handle[Unit]: (a: A, t: Throwable) =>
            t match
              case _: CancellationException =>
                future.cancelToFuture() // Runs in asynchronously background ???
                ()
              case _ =>

        case _ =>

      javaFuture
