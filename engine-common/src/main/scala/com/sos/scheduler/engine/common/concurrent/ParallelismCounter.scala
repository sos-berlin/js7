package com.sos.scheduler.engine.common.concurrent

import java.lang.Math._
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
final class ParallelismCounter {

  private val inParallel = new AtomicInteger
  private var _maximum = 0
  private var _total = 0
  private val lock = new Object

  def apply[A](body: ⇒ A): A = {
    begin()
    val result = body
    end()
    result
  }

  private def begin(): Unit = {
    lock.synchronized {
      val i = inParallel.incrementAndGet()
      _maximum = max(_maximum, i)
      _total += 1
    }
  }

  private def end(): Unit = {
    inParallel.decrementAndGet()
  }

  def maximum = _maximum

  def total = _total
}

object ParallelismCounter {
  def expect[A](parallelismMaximum: Int)(body: ParallelismCounter ⇒ A): A =
    expect(Some(parallelismMaximum), None)(body)

  def expect[A](parallelismMaximum: Int, total: Int)(body: ParallelismCounter ⇒ A):A =
    expect(Some(parallelismMaximum), Some(total))(body)

  def expect[A](parallelismMaximum: Option[Int], total: Option[Int])(body: ParallelismCounter ⇒ A):A = {
    val counter = new ParallelismCounter
    val result = body(counter)
    for (o ← total; if counter.total != o)
      throw new RuntimeException(s"Total number of excecutions ${counter.total} != expected $o")
    for (o ← parallelismMaximum; if counter.maximum != o)
      throw new RuntimeException(s"Parallelism maximum ${counter.maximum} != expected $o")
    result
  }
}
