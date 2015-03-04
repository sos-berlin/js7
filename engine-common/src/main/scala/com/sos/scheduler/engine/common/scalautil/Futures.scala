package com.sos.scheduler.engine.common.scalautil

import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
object Futures {

  object implicits {

    implicit class SuccessFuture[A](val delegate: Future[A]) extends AnyVal {
      def successValue: A = delegate.value match {
        case Some(t: Try[A]) ⇒ t.get
        case None ⇒ throw new FutureNotSucceededException
      }
    }

    implicit class SuccessPromise[A](val delegate: Promise[A]) extends AnyVal {
      def successValue: A = delegate.future.successValue
    }
  }

  class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")
}
