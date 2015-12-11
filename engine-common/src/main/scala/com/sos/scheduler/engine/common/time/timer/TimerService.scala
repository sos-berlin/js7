package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.scalautil.Futures.NoFuture
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.Timer.nowMillis
import com.sos.scheduler.engine.common.time.timer.TimerService._
import java.lang.Math.addExact
import java.time.Instant.now
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicBoolean
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.concurrent._
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class TimerService(precision: Duration, idleTimeout: Option[Duration] = None)(implicit ec: ExecutionContext) extends AutoCloseable {

  private val precisionMillis = precision.toMillis max 1
  private val queue = new ConcurrentOrderedQueue(new TreeMapOrderedQueue({ a: Timer[_] ⇒ millisToKey(a.atEpochMilli): java.lang.Long }))
  @volatile private var closed = false

  private object clock {
    private val logger = Logger(getClass)
    private val lock = new Object
    private val _isRunning = new AtomicBoolean
    @volatile private var stopped = false

    def stop(): Unit = {
      stopped = true
      wake()
    }

    def onAdded(): Unit = startOrWake()

    def startOrWake(): Unit = {
      if (_isRunning.compareAndSet(false, true)) {
        Future {
          blocking {
            try loop()
            catch { case t: Throwable ⇒
              logger.error(s"$t", t)
              throw t
            }
            finally _isRunning.set(false)
          }
        }
      } else {
        wake()
      }
    }

    def wake(): Unit = lock.synchronized { lock.notifyAll() }

    private def loop(): Unit = {
      var timedout = false
      var hot = false
      while (!stopped && !timedout) {
        queue.popNext(millisToKey(nowMillis())) match {
          case Left(key) ⇒
            if (key == neverKey) {
              hot = false
              timedout = idleUntilTimeout()
            } else {
              keyToMillis(key) - nowMillis() match {
                case duration if duration > 0 ⇒
                  hot = false
                  lock.synchronized { lock.wait(duration) }
                case d ⇒
                  if (hot) {
                    logger.warn(s"queue.popNext returns $d")
                    Thread.sleep(precisionMillis max 100)
                  }
                  hot = true
              }
            }
          case Right(timer) ⇒
            hot = false
            timer.complete()
        }
      }
    }

    private def idleUntilTimeout(): Boolean = {
      lock.synchronized {
        idleTimeout match {
          case Some(d) ⇒ lock.wait(d.toMillis)
          case None ⇒ lock.wait()
        }
      }
      isEmpty && idleTimeout.isDefined
    }

    def isRunning = _isRunning.get
  }

  private val neverTimer = new Timer[Nothing](Instant.ofEpochMilli((Long.MaxValue / precisionMillis - 1) * precisionMillis), "Never")
  private val neverKey = millisToKey(neverTimer.atEpochMilli)
  queue add neverTimer  // Marker at end of the never empty queue

  def close(): Unit = {
    closed = true
    clock.stop()
    queue.clear()
  }

  def delay(delay: Duration, name: String): Timer[Unit] =
    at(now + delay, name)

  def delay[A, B](delay: Duration, name: String, cancelWhenCompleted: Future[B]): Timer[A] =
    at2(now + delay, name, cancelWhenCompleted = cancelWhenCompleted)

  def at(at: Instant, name: String): Timer[Unit] =
    add(new Timer[Unit](at, name))

  def at[B](at: Instant, name: String, cancelWhenCompleted: Future[B]): Timer[Unit] =
    at2(at, name, cancelWhenCompleted = cancelWhenCompleted)

  private[timer] def at2[A, B](
    at: Instant,
    name: String,
    completeWith: Try[A] = Timer.ElapsedFailure,
    promise: Promise[A] = Promise[A](),
    cancelWhenCompleted: Future[B] = NoFuture): Timer[A] =
  {
    val timer = new Timer(at, name, completeWith, promise)
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

  private def add[A](timer: Timer[A]): timer.type = {
    require(millisToKey(timer.at.toEpochMilli) < neverKey)
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
    queue.remove(millisToKey(timer.atEpochMilli), timer)
    clock.wake()
  }

  override def toString = "TimerService" + (if (isEmpty) "" else s"(${queue.head.at}: ${queue.size} timers)") mkString ""

  def isEmpty = queue.isEmpty/*when closed*/ || queue.head.atEpochMilli == neverTimer.atEpochMilli

  def overview: TimerServiceOverview = TimerServiceOverview(
    count = queue.size - 1,
    first = queue.headOption filterNot isEndMark map timerToOverview,
    last = (queue.toSeq dropRight 1).lastOption map timerToOverview)  // O(queue.size) !!!

  def timerOverviews: immutable.Seq[TimerOverview] = queue.toSeq filterNot isEndMark map timerToOverview

  private def nextInstant = queue.head.at

  private[timer] def isRunning = clock.isRunning

  private def isEndMark[A](timer: Timer[_]) = timer.atEpochMilli == neverTimer.atEpochMilli

  private def millisToKey(millis: Long) = addExact(millis, precisionMillis - 1) / precisionMillis

  private def keyToMillis(key: Long) = key * precisionMillis
}

object TimerService {
  private def timerToOverview(timer: Timer[_]) = TimerOverview(timer.at, name = timer.name)

  implicit class TimeoutFuture[A](val delegate: Future[A]) extends AnyVal {
    def timeoutAfter[B >: A](delay: Duration, name: String, completeWith: Try[B] = Timer.ElapsedFailure)(implicit timerService: TimerService, ec: ExecutionContext): Future[B] =
      timeoutAt(now + delay, name)

    def timeoutAt[B >: A](at: Instant, name: String, completeWith: Try[B] = Timer.ElapsedFailure)(implicit timerService: TimerService, ec: ExecutionContext): Future[B] = {
      val promise = Promise[B]()
      delegate onComplete promise.tryComplete
      timerService.at2(at, name, cancelWhenCompleted = promise.future, promise = promise, completeWith = completeWith)
      promise.future
    }
  }
}
