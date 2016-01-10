package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.Timer.nowMillis
import com.sos.scheduler.engine.common.time.timer.TimerService._
import com.sos.scheduler.engine.common.time.timer.signaling.SynchronizedSignaling
import java.lang.System.currentTimeMillis
import java.time.Instant.now
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicBoolean
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.concurrent._
import scala.util.Try
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class TimerService(runInBackground: (⇒ Unit) ⇒ Unit, idleTimeout: Option[Duration] = None) extends AutoCloseable {

  private val queue = new ConcurrentOrderedQueue(new TreeMapOrderedQueue({ a: Timer[_] ⇒ a.atEpochMilli: java.lang.Long }))
  @volatile private var closed = false
  @volatile private var elapsedCounter = 0
  @volatile private var timerCompleteCounter = 0

  private object clock {
    private val logger = Logger(getClass)
    private val headChanged = new SynchronizedSignaling
    private val _isRunning = new AtomicBoolean
    @volatile private var stopped = false

    def stop(): Unit = {
      stopped = true
      wake()
    }

    def onAdded(): Unit = startOrWake()

    def startOrWake(): Unit = {
      if (_isRunning.compareAndSet(false, true)) {
        runInBackground {
          try loop()
          catch { case t: Throwable ⇒
            logger.error(s"$t", t)
            throw t
          }
          finally _isRunning.set(false)
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
          waitUntil - nowMillis() match {
            case remainingMillis if remainingMillis > 0 ⇒
              hot = false
              val t = currentTimeMillis
              headChanged.awaitMillis(remainingMillis)
              if (currentTimeMillis - t >= remainingMillis) elapsedCounter += 1  // Time elapsed (probably no notifyAll)
            case d ⇒
              if (hot) {
                logger.warn(s"queue.popNext returns $d")
                Thread.sleep(100)
              }
              hot = true
          }
        }
      }
      var timedout = false
      while (!stopped && !timedout) {
        queue.popNext(nowMillis()) match {
          case Left(atMillis) ⇒
            if (atMillis == neverMillis) {
              waitUntil.hot = false
              timedout = idleUntilTimeout()
            } else {
              waitUntil(atMillis)
            }
          case Right(timer) ⇒
            waitUntil.hot = false
            timerCompleteCounter += 1
            timer.complete()
        }
      }
    }

    private def idleUntilTimeout(): Boolean =
      idleTimeout match {
        case Some(d) ⇒
          val signaled = headChanged.awaitMillis(d.toMillis)
          !signaled
        case None ⇒
          headChanged.await()
          true
      }

    def isRunning = _isRunning.get
  }

  private val neverTimer = new Timer[Nothing](Instant.ofEpochMilli(Long.MaxValue), "Never")
  private val neverMillis = neverTimer.atEpochMilli
  queue add neverTimer  // Marker at end of the never empty queue

  def close(): Unit = {
    closed = true
    clock.stop()
    logger.debug("close " + overview.toJson.compactPrint)
    queue.clear()
  }

  def delay(delay: Duration, name: String): Timer[Unit] =
    at(now + delay, name)

  def delay[B](delay: Duration, name: String, cancelWhenCompleted: Future[B])(implicit ec: ExecutionContext): Timer[Unit] =
    at(now + delay, name, cancelWhenCompleted)

  def at(at: Instant, name: String): Timer[Unit] =
    add(new Timer[Unit](chooseWakeTime(at), name))

  def at[B](at: Instant, name: String, cancelWhenCompleted: Future[B])(implicit ec: ExecutionContext): Timer[Unit] =
    add(new Timer[Unit](chooseWakeTime(at), name), cancelWhenCompleted)

  private def add[A, B](timer: Timer[A], cancelWhenCompleted: Future[B])(implicit ec: ExecutionContext): timer.type = {
    if (cancelWhenCompleted.isCompleted) {
      timer.cancel()
    } else {
      add(timer)
      cancelWhenCompleted onComplete { _ ⇒
        cancel(timer)
      }
    }
    timer
  }

  private[timer] def add[A](timer: Timer[A]): timer.type = {
    require(timer.at.toEpochMilli < neverMillis)
    requireState(!closed)
    val t = nextInstant
    queue.add(timer)
    if (timer.at < t) {
      clock.onAdded()
    }
    timer
  }

  def cancel[A](timer: Timer[A]): Unit = {
    timer.cancel()
    queue.remove(timer.atEpochMilli, timer)
    clock.wake()
  }

  override def toString = "TimerService" + (if (isEmpty) "" else s"(${queue.head.at}: ${queue.size} timers)") mkString ""

  def isEmpty: Boolean = queue.headOption.fold(true) { _.atEpochMilli == neverTimer.atEpochMilli }

  def overview: TimerServiceOverview = TimerServiceOverview(
    elapsedCount = elapsedCounter,
    completeCount = timerCompleteCounter,
    count = queue.size - 1,
    first = queue.headOption filterNot isEndMark map timerToOverview,
    last = (queue.toSeq dropRight 1).lastOption map timerToOverview)  // O(queue.size) !!!

  def timerOverviews: immutable.Seq[TimerOverview] = queue.toSeq filterNot isEndMark map timerToOverview

  private def nextInstant = queue.head.at

  private[timer] def isRunning = clock.isRunning

  private def isEndMark[A](timer: Timer[_]) = timer.atEpochMilli == neverTimer.atEpochMilli
}

object TimerService {
  private val logger = Logger(getClass)

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
      timeoutAt(now + delay, name)

    def timeoutAt[B >: A](at: Instant, name: String, completeWith: Try[B] = Timer.ElapsedFailure)(implicit timerService: TimerService, ec: ExecutionContext): Future[B] = {
      val promise = Promise[B]()
      delegate onComplete promise.tryComplete
      timerService.add(new Timer(at, name, promise = promise, completeWith = completeWith), cancelWhenCompleted = promise.future)
      promise.future
    }
  }
}
