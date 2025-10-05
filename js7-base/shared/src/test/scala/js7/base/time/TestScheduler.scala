package js7.base.time

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.CatsBlocking.unsafeRunSyncX
import js7.base.time.ScalaTime.*
import js7.base.time.TestScheduler.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.{AsyncLock, Atomic}
import scala.annotation.unused
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.boundary.break
import scala.util.{NotGiven, Random, boundary}

private final class TestScheduler(start: Timestamp, ioRuntime: IORuntime)
  extends Scheduler:

  private val clockLock = AsyncLock()
  private val queue = mutable.SortedSet.empty[Task]
  @volatile private var _state = State(0, start.toEpochMilli)
  private val nextId = Atomic(1L)

  private given IORuntime = ioRuntime

  logger.info(s"new ⏰ ${now()} _monotonicNanos=${_state.monotonicNanos}")

  def sleep(delay: FiniteDuration, runnable: Runnable): Runnable =
    val id = nextId.getAndIncrement()
    val task = Task(_state.monotonicNanos + delay.toNanos, id, runnable)
    logger.trace(s"⏰ sleep $task")
    synchronized:
      queue += task
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

  def tick(duration: FiniteDuration): Unit =
    syncLockClock:
      val tasks =
        synchronized:
          if duration.isZero then
            logger.info(s"⏰ tick ${now()}")
          else
            _state = _state.copy(
              monotonicNanos = _state.monotonicNanos + duration.toNanos)
            val plus = duration.isPositive ?? "+"
            logger.info(s"⏰ tick ${now()} $plus${duration.pretty}")
          awokenTasks()

      // Shuffle ripe times like in reality
      for task <- Random.shuffle(tasks) do
        logger.trace(s"🔔 tick: run $task")
        task.runnable.run() // Exception? Let it crash! OTHER TASKS ARE LOST THEN!

  private def awokenTasks(): Vector[Task] =
    logTasks(indent = true)
    val tasks = VectorBuilder[Task]
    val now = _state.monotonicNanos
    boundary:
      while queue.nonEmpty do
        val task = queue.head
        if now < task.at then break()
        tasks += task
        queue.remove(task)
    tasks.result()

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
      val state = _state
      val current = state.monotonicNanos
      val plus = (at <= state.monotonicNanos) ?? "+" // Plus means due (=0) or overdue (>0)
      s"${Timestamp.ofEpochMilli(state.wallTimeShift + at / 1_000_000)}" +
        s" due=$plus${(state.monotonicNanos - at).ns.pretty} $runnable"


object TestScheduler:
  private val logger = Logger[this.type]

  logger.forceImportExtensions // Compilable for both JVM and JS

  private case class State(monotonicNanos: Long, wallTimeShift: Long):
    def nowMillis: Long =
      wallTimeShift + monotonicNanos / 1_000_000
