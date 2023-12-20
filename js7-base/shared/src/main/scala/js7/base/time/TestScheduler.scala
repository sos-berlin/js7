package js7.base.time

import cats.effect.unsafe.Scheduler
import js7.base.log.Logger
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.concurrent.duration.FiniteDuration

@deprecated
final class TestScheduler extends Scheduler:

  private val nanos = Atomic(0L)

  def sleep(delay: FiniteDuration, task: Runnable): Runnable =
    ???

  /** The real wall clock time. */
  def nowMillis(): Long =
    System.currentTimeMillis()

  def monotonicNanos(): Long =
    nanos.get

  def tick(duration: FiniteDuration): Unit =
    this.nanos += duration.toNanos

  @deprecated
  def tick(): Unit =
    ??? // FIXME Kick off waiting operations in IORuntime ?


object TestScheduler:
  private val logger = Logger(getClass)
