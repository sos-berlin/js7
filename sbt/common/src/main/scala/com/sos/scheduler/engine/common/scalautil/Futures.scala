package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.base.utils.StackTraces._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import java.util.concurrent.TimeoutException
import scala.collection.generic.CanBuildFrom
import scala.concurrent._
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
object Futures {

  private val logger = Logger(getClass)

  /**
   * Like [[Await]]`.result` - in case of exception, own stack trace is added.
   */
  def awaitResult[A](future: Future[A], atMost: Duration): A = {
    import implicits.SuccessFuture
    Await.ready[A](future, atMost.toConcurrent.toCoarsest).successValue
  }

  /**
   * Maps an exception of body to Future.failed.
   */
  def catchInFuture[A](body: ⇒ Future[A]): Future[A] =
    try body
    catch {
      case NonFatal(t) ⇒ Future.failed(t)
    }

  /**
   * A Future that will never happen.
   */
  object NoFuture extends Future[Nothing] {
    def onComplete[U](f: (Try[Nothing]) ⇒ U)(implicit ec: ExecutionContext) = {}
    def isCompleted = false
    def value = None
    def result(atMost: duration.Duration)(implicit permit: CanAwait) = throw new TimeoutException("NoFuture")
    def ready(atMost: duration.Duration)(implicit permit: CanAwait) = throw new TimeoutException("NoFuture")
  }

  object implicits {

    implicit class SuccessFuture[A](val delegate: Future[A]) extends AnyVal {
      /**
       * Like .value.get.get - in case of exception own stack trace is added.
       */
      def successValue: A = delegate.value match {
        case Some(Success(o)) ⇒ o
        case Some(Failure(t)) ⇒ throw t.appendCurrentStackTrace
        case None ⇒ throw new FutureNotSucceededException
      }

      /**
       * Returns a new Future with the current stack trace added in case of failure of the original Future.
       */
      def appendCurrentStackTrace: Future[A] = {
        val callersStackTrace = newStackTrace()
        delegate.recoverWith {
          case t ⇒ Future.failed[A](t.appendStackTrace(callersStackTrace))
        } (SynchronousExecutionContext)
      }

      def await(duration: Duration) = Await.ready(delegate, duration.toFiniteDuration).successValue
    }

    implicit class RichFutures[A, M[X] <: TraversableOnce[X]](val delegate: M[Future[A]]) extends AnyVal {
      def await(duration: Duration)(implicit ec: ExecutionContext, cbf: CanBuildFrom[M[Future[A]], A, M[A]]): M[A] =
        Await.result(Future.sequence(delegate)(cbf, ec), duration.toFiniteDuration)
    }

    implicit class RichFutureFuture[A](val delegate: Future[Future[A]]) extends AnyVal {
      def flatten: Future[A] = delegate.flatMap(identity)(SynchronousExecutionContext)
    }

    implicit class SuccessPromise[A](val delegate: Promise[A]) extends AnyVal {
      def successValue: A = delegate.future.successValue
    }
  }

  class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")

  object SynchronousExecutionContext extends ExecutionContext {
    def execute(runnable: Runnable) = runnable.run()
    def reportFailure(cause: Throwable) = logger.error(s"$cause", cause)
  }
}
