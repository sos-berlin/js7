package com.sos.scheduler.engine.common.scalautil

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
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
    Await.ready[A](future, atMost).successValue
  }

  object implicits {

    implicit class SuccessFuture[A](val delegate: Future[A]) extends AnyVal {
      /**
       * Like .value.get.get - in case of exception own stack trace is added.
       */
      def successValue: A = delegate.value match {
        case Some(Success(o)) ⇒ o
        case Some(Failure(t)) ⇒
          t.setStackTrace(t.getStackTrace ++ (new Exception).getStackTrace)
          throw t
        case None ⇒ throw new FutureNotSucceededException
      }
    }

    implicit class SuccessPromise[A](val delegate: Promise[A]) extends AnyVal {
      def successValue: A = delegate.future.successValue
    }
  }

  class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")
}
