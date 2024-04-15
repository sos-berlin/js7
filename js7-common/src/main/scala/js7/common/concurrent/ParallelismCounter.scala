package js7.common.concurrent

import java.lang.Math.*
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
final class ParallelismCounter:

  private val inParallel = new AtomicInteger
  @volatile private var _maximum = 0
  @volatile private var _total = 0
  private val lock = new Object

  def apply[A](body: => A): A =
    begin()
    val result = body
    end()
    result

  private def begin(): Unit =
    lock.synchronized:
      val i = inParallel.incrementAndGet()
      _maximum = max(_maximum, i)
      _total += 1

  private def end(): Unit =
    inParallel.decrementAndGet()

  def maximum: Int =
    _maximum

  def total: Int =
    _total


object ParallelismCounter:
  def expect[A](parallelismMaximum: Int)(body: ParallelismCounter => A): A =
    expect(Some(parallelismMaximum), None)(body)

  def expect[A](parallelismMaximum: Int, total: Int)(body: ParallelismCounter => A):A =
    expect(Some(parallelismMaximum), Some(total))(body)

  def expect[A](parallelismMaximum: Option[Int], total: Option[Int])(body: ParallelismCounter => A):A =
    val counter = new ParallelismCounter
    val result = body(counter)
    for o <- total; if counter.total != o do
      throw new RuntimeException(s"Total number of executions ${counter.total} != expected $o")
    for o <- parallelismMaximum; if counter.maximum != o do
      throw new RuntimeException(s"Parallelism maximum ${counter.maximum} != expected $o")
    result
