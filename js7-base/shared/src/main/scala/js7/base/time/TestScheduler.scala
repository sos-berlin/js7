package js7.base.time

import cats.effect.IO
import cats.effect.unsafe.Scheduler
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.TestScheduler.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.boundary.break
import scala.util.{NotGiven, Random, boundary}

private final class TestScheduler(start: Timestamp = DefaultStartTimestamp)
  extends Scheduler:

  private val queue = mutable.SortedSet.empty[Task]
  @volatile private var _monotonicNanos = 0L
  /** Difference between wall time and _monotonicNanos. */
  @volatile private var _wallTimeShift = start.toEpochMilli
  private val nextId = Atomic(1L)

  logger.info(s"new ⏰ ${now()} _monotonicNanos=$_monotonicNanos")

  def sleep(delay: FiniteDuration, runnable: Runnable): Runnable =
    val id = nextId.getAndIncrement()
    val task = Task(_monotonicNanos + delay.toNanos, id, runnable)
    logger.trace(s"⏰ schedule $task")
    synchronized:
      queue += task
    logTasks(indent = true)
    // Return the cancel operation
    () =>
      synchronized:
        queue -= task

  def monotonicNanos(): Long =
    _monotonicNanos

  def nowMillis(): Long =
    synchronized:
      _wallTimeShift + _monotonicNanos / 1_000_000

  def now(): Timestamp =
    Timestamp.ofEpochMilli(nowMillis())

  def lock[A](body: => A)(using NotGiven[A <:< IO[?]]): A =
    synchronized(body)

  /** Reset the wall clock but not the monotonic clock (move the clock hands). */
  def resetNow(timestamp: Timestamp): Unit =
    synchronized:
      _wallTimeShift += (timestamp - now()).toMillis
      logger.info(s"resetNow ⏰ := ${now()}")
    logTasks(indent = true)

  def tick(duration: FiniteDuration): Unit =
    val tasks =
      synchronized:
        _monotonicNanos += duration.toNanos
        val plus = duration.isPositive ?? "+"
        logger.info(s"⏰ := ${now()} $plus${duration.pretty}")
        awokenTasks()

    // Shuffle ripe times like in reality
    for task <- Random.shuffle(tasks) do
      logger.trace(s"🔔 run $task")
      task.runnable.run() // Exception? Let it crash! OTHER TASK ARE LOST THEN!

  private def awokenTasks(): Vector[Task] =
    val tasks = VectorBuilder[Task]
    val now = _monotonicNanos
    boundary:
      while queue.nonEmpty do
        val task = queue.head
        if now < task.at then break()
        tasks += task
        queue.remove(task)
    tasks.result()

  private def logTasks(indent: Boolean = false): Unit =
    logger.whenTraceEnabled:
      val tasks = synchronized(Vector.from(queue))
      if tasks.nonEmpty then
        var prefix, lastPrefix = ""
        if indent then
          prefix = "│ " //"├╴"
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
      val current = _monotonicNanos
      val plus = (at <= current) ?? "+" // Plus means due (=0) or overdue (>0)
      s"${Timestamp.ofEpochMilli(_wallTimeShift + at / 1_000_000)}" +
        s" due=$plus${(current - at).ns.pretty} $runnable"


object TestScheduler:
  private val logger = Logger[this.type]
  private val DefaultStartTimestamp = Timestamp("2000-01-01T00:00:00Z")

  logger.forceImportExtensions // Compilable for both JVM and JS
