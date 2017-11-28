package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.utils.StackTraces._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Duration
import scala.collection.generic.CanBuildFrom
import scala.concurrent._
import scala.concurrent.duration.Duration.Inf
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

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
    * A Future for blocking code.
    * A big number of blocking Futures may be started, each consuming a thread.
    * This differs from ExecutionContext.global, which limits the number of threads to (number of cores) + 256.
    */
  def blockingFuture[A](body: ⇒ A): Future[A] =
    namedThreadFuture("")(body)

  def namedThreadFuture[A](name: String)(body: ⇒ A): Future[A] = {
    val promise = Promise[A]()
    new Thread {
      if (name.nonEmpty) setName(name)
      override def run() =
        try promise.success(body)
        catch { case t: Throwable ⇒  // Not only NonFatal (or what should we do else?)
          promise.failure(t)
        }
    } .start()
    promise.future
  }

  def promiseFuture[A](body: Promise[A] ⇒ Unit): Future[A] = {
    val promise = Promise[A]()
    body(promise)
    promise.future
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
        val callersStackTrace = new Exception().getStackTrace
        delegate.recoverWith {
          case t ⇒ Future.failed[A](t.appendStackTrace(callersStackTrace))
        } (SynchronousExecutionContext)
      }

      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[Duration]): A =
        duration match {
          case Some(o) ⇒ await(o)
          case None ⇒ awaitInfinite
        }

      def await(duration: Duration): A =
        Await.ready(delegate, duration.toFiniteDuration).successValue

      def awaitInfinite: A =
        Await.ready(delegate, Inf).successValue
    }

    implicit class RichFutures[A, M[X] <: TraversableOnce[X]](val delegate: M[Future[A]]) extends AnyVal {
      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[Duration])(implicit ec: ExecutionContext, cbf: CanBuildFrom[M[Future[A]], A, M[A]]): M[A] =
        duration match {
          case Some(o) ⇒ await(o)
          case None ⇒ awaitInfinite
        }

      def await(duration: Duration)(implicit ec: ExecutionContext, cbf: CanBuildFrom[M[Future[A]], A, M[A]]): M[A] =
        Await.result(Future.sequence(delegate)(cbf, ec), duration.toFiniteDuration)

      def awaitInfinite(implicit ec: ExecutionContext, cbf: CanBuildFrom[M[Future[A]], A, M[A]]): M[A] =
        Await.ready(Future.sequence(delegate)(cbf, ec), Inf).successValue
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
