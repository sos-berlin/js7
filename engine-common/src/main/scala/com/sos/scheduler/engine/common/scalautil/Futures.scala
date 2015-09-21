package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Tries.{extendStackTraceWith, newStackTrace}
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * @author Joacim Zschimmer
 */
object Futures {

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
  val NoFuture: Future[Nothing] = Promise().future

  object implicits {

    implicit class SuccessFuture[A](val delegate: Future[A]) extends AnyVal {
      /**
       * Like .value.get.get - in case of exception own stack trace is added.
       */
      def successValue: A = delegate.value match {
        case Some(Success(o)) ⇒ o
        case Some(Failure(t)) ⇒
          extendStackTraceWith(t, newStackTrace())
          throw t
        case None ⇒ throw new FutureNotSucceededException
      }

      /**
       * Returns a new Future with the own stack trace added in case of failure of the original Future.
       */
      def withThisStackTrace(implicit ec: ExecutionContext): Future[A] = {
        val callersStackTrace = newStackTrace()
        delegate recoverWith {
          case t ⇒
            extendStackTraceWith(t, callersStackTrace)
            Future.failed[A](t)
        }
      }
    }

    implicit class SuccessPromise[A](val delegate: Promise[A]) extends AnyVal {
      def successValue: A = delegate.future.successValue
    }
  }

  class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")
}
