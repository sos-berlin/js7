package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.base.utils.ScalaUtils.someUnless
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.YamlJsonConversion.ToYamlString
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.Timer.nowMillis
import com.sos.scheduler.engine.common.time.timer.TimerService._
import com.sos.scheduler.engine.common.time.timer.signaling.SynchronizedSignaling
import java.lang.System.currentTimeMillis
import java.time.Instant.now
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicBoolean
import org.jetbrains.annotations.TestOnly
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.concurrent._
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
final class TimerService(runInBackground: (⇒ Unit) ⇒ Unit, idleTimeout: Option[Duration] = None) extends AutoCloseable {

  private val queue = new ConcurrentOrderedQueue(new TreeMapOrderedQueue({ a: Timer[_] ⇒ a.atEpochMilli: java.lang.Long }))
  @volatile private var closed = false
  @volatile private var timerCompleteCounter = 0
  @volatile private var wakeCounter = 0
  @volatile private var prematureWakeCounter = 0

  private object clock {
    private val headChanged = new SynchronizedSignaling
    private val _isRunning = new AtomicBoolean
    @volatile private var stopped = false

    def stop(): Unit = {
      stopped = true
      wake()
    }

    def startOrWake(): Unit = {
      if (_isRunning.compareAndSet(false, true)) {
        runInBackground {
          logger.debug("Started")
          try loop()
          catch { case t: Throwable ⇒
            logger.error(s"$t", t)
            throw t
          }
          finally {
            logger.debug("Terminated")
            _isRunning.set(false)
            if (!stopped && nonEmpty) startOrWake()
          }
        }
      } else {
        wake()
      }
    }

    def wake(): Unit = headChanged.signal()

    private def loop(): Unit = {
      object waitUntil {
        var hot = false
        def apply(waitUntil: Long): Unit = {
          waitUntil - nowMillis match {
            case remainingMillis if remainingMillis > 0 ⇒
              hot = false
              val signaled = headChanged.awaitMillis(remainingMillis)
              if (!signaled) wakeCounter += 1
            case d ⇒
              if (hot) {
                prematureWakeCounter += 1
                logger.warn(s"queue.popNext returns $d")
                Thread.sleep(100)
              }
              hot = true
          }
        }
      }
      var timedout = false
      while (!stopped && !timedout) {
        queue.popNext(nowMillis) match {
          case Left(atMillis) ⇒
            if (atMillis == NeverMillis) {
              waitUntil.hot = false
              timedout = idleUntilTimeout()
            } else {
              waitUntil(atMillis)
            }
          case Right(timer) ⇒
            waitUntil.hot = false
            timer.complete()
            timerCompleteCounter += 1
        }
      }
      if (stopped) logger.debug("Stopped")
      if (timedout) logger.debug("Timedout")
    }

    private def idleUntilTimeout(): Boolean = {
      val signaled = idleTimeout match {
        case Some(d) ⇒
          headChanged.awaitMillis(d.toMillis)
        case None ⇒
          headChanged.await()
          true
      }
      if (!signaled) wakeCounter += 1
      !signaled
    }

    def isRunning = _isRunning.get
  }

  queue.add(NeverTimer)  // Marker at end of the never empty queue

  def close(): Unit = {
    closed = true
    clock.stop()
    logger.debug(s"close $toString")
    queue.clear()
  }

  /**
    * Tries to complete `promise` with `completeWith` after timeout.
    */
  def tryCompletePromiseAfter[A](delay: Duration, promise: Promise[A], completeWith: Try[A], name: String)(implicit ec: ExecutionContext): Unit =
    tryCompletePromiseAt(now + delay, promise, completeWith, name)

  /**
    * Tries to complete `promise` with `completeWith` after timeout.
    */
  def tryCompletePromiseAt[A](at: Instant, promise: Promise[A], completeWith: Try[A], name: String)(implicit ec: ExecutionContext): Unit = {
    val timer = add(new Timer(chooseWakeTime(at), name, promise = promise, completeWith = completeWith))
    promise.future onComplete { _ ⇒ cancel(timer) }
  }

  def delay(delay: Duration, name: String): Timer[Unit] =
    at((now plusNanos 1000) + delay, name)

  def at(at: Instant, name: String): Timer[Unit] =
    add(new Timer[Unit](chooseWakeTime(at), name))

  private[timer] def add[A](timer: Timer[A]): timer.type = {
    require(timer.at.toEpochMilli < NeverMillis)
    requireState(!closed)
    if (timer.at <= now) {
      timer.complete()
      timerCompleteCounter += 1
    } else {
      enqueue(timer)
    }
    timer
  }

  private def enqueue[A](timer: Timer[A]): Unit = {
    val t = nextInstant
    queue.add(timer)
    if (timer.at < t) {
      clock.startOrWake()
    }
  }

  /**
    * @return true when cancelled
    */
  def cancel[A](timer: Timer[A]): Boolean = {
    timer.cancel()
    val removed = queue.remove(timer.atEpochMilli, timer)
    clock.wake()
    removed
  }

  override def toString = overview.toFlowYaml

  def isEmpty: Boolean = !nonEmpty

  def nonEmpty: Boolean = queue.headOption exists { _ != NeverTimer }

  @TestOnly
  def queueSize: Int = queue.size

  def overview: TimerServiceOverview = TimerServiceOverview(
    count = queue.size - 1,
    completeCount = timerCompleteCounter,
    wakeCount = wakeCounter,
    prematureWakeCount = someUnless(prematureWakeCounter, 0),
    first = queue.headOption filterNot isEndMark map timerToOverview,
    last = (queue.toSeq dropRight 1).lastOption map timerToOverview)  // O(queue.size) !!!

  def timerOverviews: immutable.Seq[TimerOverview] = queue.toSeq filterNot isEndMark map timerToOverview

  private def nextInstant = queue.head.at

  private[timer] def isRunning = clock.isRunning

  private def isEndMark[A](timer: Timer[_]) = timer == NeverTimer
}

object TimerService {
  private val logger = Logger(getClass)
  private val NeverTimer = new Timer[Nothing](Instant.ofEpochMilli(Long.MaxValue), "Never")
  private val NeverMillis = NeverTimer.atEpochMilli

  def apply(idleTimeout: Option[Duration] = None)(implicit ec: ExecutionContext) =
    new TimerService(runInBackground = body ⇒ Future { blocking { body } }, idleTimeout)

  /**
    * Coalesces wake-up times to reduce processor wake-ups.
    * @see http://msdn.microsoft.com/en-us/library/windows/hardware/gg463266.aspx
    */
  private def chooseWakeTime(at: Instant): Instant = roundUp(at, currentTimeMillis = currentTimeMillis)

  private[timer] def roundUp(at: Instant, currentTimeMillis: Long): Instant = {
    val atMillis = at.toEpochMilli
    val round = roundUp(atMillis - currentTimeMillis)
    Instant.ofEpochMilli((atMillis + round - 1) / round * round)
  }

  private def roundUp(millis: Long): Long =
    millis match {
      case d if d >= 10000 ⇒ 1000
      case d if d >= 5000 ⇒ 500
      case d if d >= 1000 ⇒ 100
      case d if d >= 500 ⇒ 50
      case d if d >= 100 ⇒ 10
      case _ ⇒ 1
    }

  private def timerToOverview(timer: Timer[_]) = TimerOverview(timer.at, name = timer.name)

  implicit class TimeoutFuture[A](val delegate: Future[A]) extends AnyVal {
    def timeoutAfter[B >: A](delay: Duration, name: String, completeWith: Try[B] = Timer.ElapsedFailure)(implicit timerService: TimerService, ec: ExecutionContext): Future[B] =
      timeoutAt(now + delay, completeWith, name)

    def timeoutAt[B >: A](at: Instant, completeWith: Try[B] = Timer.ElapsedFailure, name: String)(implicit timerService: TimerService, ec: ExecutionContext): Future[B] = {
      val promise = Promise[B]()
      delegate onComplete promise.tryComplete
      timerService.tryCompletePromiseAt(at, promise, completeWith, name = name)
      promise.future
    }

    def delay(duration: Duration)(implicit timerService: TimerService, ec: ExecutionContext): Future[A] = {
      if (duration <= Duration.ZERO)
        delegate
      else {
        val promise = Promise[A]()
        delegate onComplete {
          case Success(result) ⇒ timerService.delay(duration, name = "delay").onElapsed { promise.success(result) }
          case o @ Failure(_) ⇒ promise.complete(o)
        }
        promise.future
      }
    }
  }
}
