package com.sos.scheduler.engine.common.time.timer

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
trait DelegatedFuture[A] extends Future[A] {

  protected def delegatedFuture: Future[A]

  def onComplete[U](f: (Try[A]) ⇒ U)(implicit executor: ExecutionContext) = delegatedFuture.onComplete(f)

  def isCompleted = delegatedFuture.isCompleted

  def value = delegatedFuture.value

  def result(atMost: Duration)(implicit permit: CanAwait) = delegatedFuture.result(atMost)

  def ready(atMost: Duration)(implicit permit: CanAwait) = {
    delegatedFuture.ready(atMost)
    this
  }
}
