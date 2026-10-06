package js7.base.time

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.CatsBlocking.unsafeRunSyncX
import js7.base.time.ScalaTime.*
import js7.base.time.TestScheduler.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import scala.annotation.unused
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{NotGiven, Random}

private final class TestScheduler(start: Timestamp, ioRuntime: IORuntime)
  extends Scheduler:

  private val clockLock = AsyncLock()
  private val queue = mutable.SortedSet.empty[Task]
  @volatile private var _state = State(0, start.toEpochMilli)
  private var ticking = false // Guarded by synchronized
  private val nextId = Atomic(1L)

  private given IORuntime = ioRuntime

  logger.info(s"new ⏰ ${now()} _monotonicNanos=${_state.monotonicNanos}")

  def sleep(delay: FiniteDuration, runnable: Runnable): Runnable =
    val id = nextId.getAndIncrement()
    val (task, runNow) =
      synchronized:
        val task = Task(_state.monotonicNanos + delay.toNanos, id, runnable)
        // While ticking, tick runs due tasks itself
        val runNow = !ticking && task.at <= _state.monotonicNanos
        if !runNow then queue += task
        task -> runNow
    if runNow then
      // Like a real Scheduler, run a due task without waiting for the next tick.
      // Otherwise, the task would never run if the test has already ticked to this time.
      logger.trace(s"⏰ sleep $task: run now")
      ioRuntime.compute.execute(runnable)
      () => ()
    else
      logger.trace(s"⏰ sleep $task")
      logTasks(indent = true)
      // Return the cancel operation
      () =>
        synchronized:
          queue -= task

  def monotonicNanos(): Long =
    _state.monotonicNanos

  def nowMillis(): Long =
    _state.nowMillis

  def now(): Timestamp =
    Timestamp.ofEpochMilli(nowMillis())

  def lock[A](body: => A)(using @unused x: NotGiven[A <:< IO[?]]): A =
    syncLockClock(body)

  /** TestAlarmClock synchronizes time query and time change. */
  def lockIO[A](body: Timestamp => IO[A])(using sourcecode.Enclosing): IO[A] =
    clockLock.lock:
      IO.defer:
        body(now())

  /** Reset the wall clock but not the monotonic clock (move the clock hands). */
  def resetNow(timestamp: Timestamp): Unit =
    syncLockClock:
      synchronized:
        _state = _state.copy(
          wallTimeShift = _state.wallTimeShift + (timestamp - now()).toMillis)
        logger.info(s"resetNow ⏰ := ${now()}")
      logTasks(indent = true)

  /** Proceed the monotonic clock step by step to each due task, and run the tasks.
   * <p>
   * Tasks are run in the order of their scheduled times, each seeing its scheduled time as
   * `now()`. Tasks scheduled while ticking are run in the same tick, when due.
   * <p>
   * If tasks throw, all due tasks are run nevertheless, and the first exception is thrown.
   */
  def tick(duration: FiniteDuration): Unit =
    require(!duration.isNegative, s"TestScheduler.tick(${duration.pretty}): negative duration")
    syncLockClock:
      val target = _state.monotonicNanos + duration.toNanos
      synchronized:
        ticking = true
        logger.info:
          s"⏰ tick ${toTimestamp(target)}${duration.isPositive ?? s" +${duration.pretty}"}"
      logTasks(indent = true)

      var firstThrowable: Option[Throwable] = None
      try
        var tasks = nextDueTasks(target)
        while tasks.nonEmpty do
          // Shuffle tasks due at the same time, like in reality
          for task <- Random.shuffle(tasks) do
            logger.trace(s"🔔 tick: run $task")
            try task.runnable.run()
            catch case NonFatal(t) =>
              logger.error(s"tick: $task: ${t.toStringWithCauses}", t)
              if firstThrowable.isEmpty then firstThrowable = Some(t)
          tasks = nextDueTasks(target)
      finally
        synchronized:
          ticking = false

      firstThrowable.foreach(throw _)

  /** Advance the clock to the next due task, and return all tasks due at that time.
   * <p>
   * If no task is due until `target`, advance the clock to `target`, stop ticking, and
   * return an empty Vector. */
  private def nextDueTasks(target: Long): Vector[Task] =
    synchronized:
      queue.headOption match
        case Some(task) if task.at <= target =>
          if _state.monotonicNanos < task.at then
            _state = _state.copy(monotonicNanos = task.at)
          val now = _state.monotonicNanos
          val tasks = queue.view.takeWhile(_.at <= now).toVector
          queue --= tasks
          tasks

        case _ =>
          _state = _state.copy(monotonicNanos = target)
          ticking = false
          Vector.empty

  private def toTimestamp(monotonicNanos: Long): Timestamp =
    Timestamp.ofEpochMilli(_state.wallTimeShift + monotonicNanos / 1_000_000)

  private def syncLockClock[A](body: => A): A =
    clockLock.lock(IO(body)).unsafeRunSyncX()

  private def logTasks(indent: Boolean = false): Unit =
    logger.whenTraceEnabled:
      val tasks = synchronized(Vector.from(queue))
      if tasks.nonEmpty then
        var prefix, lastPrefix = ""
        if indent then
          prefix = "│ "
          lastPrefix = "└╴"
        val last = tasks.last

        for task <- tasks do
          val prfx = if task eq last then lastPrefix else prefix
          logger.trace(s" ${prfx}Task queue: $task")


  private final case class Task(at: Long, id: Long, runnable: Runnable) extends Ordered[Task]:
    def compare(that: Task) =
      at.compare(that.at) match
        case 0 => java.lang.Long.compare(id, that.id)
        case i => i

    override def toString =
      val current = _state.monotonicNanos
      val plus = (at <= current) ?? "+" // Plus means due (=0) or overdue (>0)
      s"${toTimestamp(at)} due=$plus${(current - at).ns.pretty} $runnable"


object TestScheduler:
  private val logger = Logger[this.type]

  logger.forceImportExtensions // Compilable for both JVM and JS

  private case class State(monotonicNanos: Long, wallTimeShift: Long):
    def nowMillis: Long =
      wallTimeShift + monotonicNanos / 1_000_000
