package com.sos.scheduler.engine.common.javautils

import java.util.concurrent.{ExecutionException, TimeUnit, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class ScalaInJavaFuture[A](val delegate: Future[A]) extends java.util.concurrent.Future[A] {

  def cancel(mayInterruptIfRunning: Boolean) =
    throw new UnsupportedOperationException("com.sos.scheduler.engine.common.javautils.ScalaInJava.WrappedFuture.cancel")

  def isCancelled = false

  def isDone = delegate.isCompleted

  @throws[InterruptedException]
  @throws[ExecutionException]
  def get(): A = get(Long.MaxValue, NANOSECONDS)  // 262 years

  @throws[InterruptedException]
  @throws[ExecutionException]
  @throws[TimeoutException]
  def get(timeout: Long, unit: TimeUnit): A =
    Await.ready(delegate, Duration(unit.toNanos(timeout), NANOSECONDS)).value.get match {
      case Success(o) ⇒ o
      case Failure(t) ⇒ throw new ExecutionException(t.toString, t)
    }
}
