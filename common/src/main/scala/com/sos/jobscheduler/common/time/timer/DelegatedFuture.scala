package com.sos.jobscheduler.common.time.timer

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
trait DelegatedFuture[A] extends Future[A] {

  protected def delegatedFuture: Future[A]

  def onComplete[U](f: (Try[A]) ⇒ U)(implicit ec: ExecutionContext) = delegatedFuture.onComplete(f)

  def isCompleted = delegatedFuture.isCompleted

  def value = delegatedFuture.value

  def result(atMost: Duration)(implicit permit: CanAwait) = delegatedFuture.result(atMost)

  def ready(atMost: Duration)(implicit permit: CanAwait) = {
    delegatedFuture.ready(atMost)
    this
  }

  def transform[S](f: Try[A] ⇒ Try[S])(implicit ec: ExecutionContext) = delegatedFuture.transform(f)

  def transformWith[S](f: Try[A] ⇒ Future[S])(implicit ec: ExecutionContext) = delegatedFuture.transformWith(f)
}
