package com.sos.scheduler.engine.common.concurrent

import java.lang.Math._
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
final class ParallelizationCounter {

  private val parallel = new AtomicInteger
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
      val i = parallel.incrementAndGet()
      _maximum = max(_maximum, i)
      _total += 1
    }
  }

  private def end(): Unit = {
    parallel.decrementAndGet()
  }

  def maximum = _maximum

  def total = _total
}

object ParallelizationCounter {
  def expect[A](parallelization: Int)(body: ParallelizationCounter ⇒ A):A = {
    val counter = new ParallelizationCounter
    val result = body(counter)
    if (counter.maximum != parallelization)
      throw new RuntimeException(s"Parallelization of ${counter.maximum}, but $parallelization is expected")
    result
  }
}
