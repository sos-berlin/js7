package js7.base.stream

import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.stream.ObservableNumberedQueue._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject
import scala.concurrent.Future
import scala.util.{Failure, Success}

final class ObservableNumberedQueue[V: Tag]
{
  private val vName = implicitly[Tag[V]].tag.toString
  private val sync = new IncreasingNumberSync(initial = 0, i => s"#$i")
  private val lock = AsyncLock("ObservableNumberedQueue", suppressLog = true)
  @volatile private var _state = State()

  def enqueue(commands: Iterable[V]): Task[Unit] =
    lock.lock(Task {
      val s = _state
      var queue = s.queue
      var nextNumber = s.nextNumber
      var lastNumber = -1L
      for (command <- commands) {
        queue :+= Numbered(nextNumber, command)
        lastNumber = nextNumber
        nextNumber += 1
      }
      _state = s.copy(queue = queue, nextNumber = nextNumber)
      if (lastNumber != -1) {
        sync.onAdded(lastNumber)
      }
    })

  def enqueueNumbered(commands: Iterable[Numbered[V]]): Task[Unit] =
    Task.when(commands.nonEmpty)(
      lock.lock(Task {
        val s = _state
        var queue = s.queue
        var nextNumber = s.nextNumber
        var lastNumber = -1L
        for (command <- commands) {
          assertThat(command.number >= nextNumber)
          queue :+= command
          lastNumber = command.number
          nextNumber = command.number + 1
        }
        _state = s.copy(queue = queue, nextNumber = nextNumber)
        if (lastNumber != -1) {
          sync.onAdded(lastNumber)
        }
      }))

  def observable: Observable[Seq[Numbered[V]]] =
    observable(after = _state.torn)

  def observable(after: Long): Observable[Seq[Numbered[V]]] =
    Observable.deferAction { implicit s =>
      val subject = PublishToOneSubject[Seq[Numbered[V]]]()

      def loop(ack: Future[Ack], after: Long): Unit =
        ack.syncTryFlatten.syncOnContinue {
          Task(_state.checkAfter(after).orThrow)
          // Should we delay some milliseconds to reduce number of command batches ???
            .*>(sync.whenAvailable(after = after, until = None, delay = 0.s))
            .*>(Task.defer(_state.readQueue(after)))
            .runToFuture  // Be sure to leave the `loop` recursion stack
            .onComplete {
              case Failure(t) =>
                subject.onError(t)

              case Success(values) =>
                if (values.isEmpty) {
                  // Race condition ???
                  logger.warn(
                    s"Internal: sync.whenAvailable($after) triggered but no command available - delay 1s")
                  Task(loop(Continue, after)).delayExecution(1.s).runAsyncAndForget
                } else {
                  val ack = subject.onNext(values)
                  loop(ack, after = values.lastOption.fold(after)(_.number))
                }
            }
        }

      // Start only when subscribed
      loop(subject.subscription, after)
      subject
    }

  def release(after: Long): Task[Checked[Unit]] =
    lock.lock(Task {
      val s = _state
      val q = s.queue
      for (_ <- s.checkAfter(after)) yield {
        val (index, found) = binarySearch(0, q.length, q(_).number.compare(after))
        _state = s.copy(torn = after, queue = q.drop(index + found.toInt))
      }
    })

  def dequeueAll: Task[Vector[Numbered[V]]] =
    lock.lock(Task {
      val s = _state
      val result = s.queue
      _state = s.copy(queue = Vector.empty)
      result
    })

  // Unused
  private def dequeueSelected(predicate: V => Boolean): Task[Vector[Numbered[V]]] =
    lock.lock(Task {
      val s = _state
      val (result, remaining) = s.queue.partition(numbered => predicate(numbered.value))
      _state = s.copy(queue = remaining)
      result
    })

  override def toString = s"ObservableNumberedQueue[$vName]"

  private sealed case class State(
    torn: Long = 0L,
    nextNumber: Long = 1L,
    queue: Vector[Numbered[V]] = Vector.empty[Numbered[V]])
  {
    def readQueue(after: Long): Task[Vector[Numbered[V]]] = {
      val q = queue
      val (index, found) = binarySearch(0, q.length, q(_).number.compare(after))
      if (!found && after != torn)
        Task.raiseError(unknownAfterProblem(after).throwable)
      else
        Task.pure(q.drop(index + found.toInt))
    }

    def requireValidNumber(after: Long): Task[Unit] = {
      val last = queue.lastOption.map(_.number)
      if (after < torn || last.exists(_ < after))
        Task.raiseError(unknownAfterProblem(after).throwable)
      else
        Task.unit
    }

    def checkAfter(after: Long): Checked[Unit] = {
      val minimum = queue.headOption.map(_.number - 1) getOrElse torn
      val last = queue.lastOption.map(_.number) getOrElse torn
      (minimum <= after && after <= last) !!
        Problem.pure(s"Unknown number: Numbered[$vName]: #$after (must be >=$minimum and <=$last)")
    }

    def unknownAfterProblem(after: Long) = {
      val beforeFirst = queue.headOption.map(_.number - 1) getOrElse torn
      val last = queue.lastOption.map(_.number) getOrElse torn
      Problem.pure(s"Unknown number: Numbered[$vName]: #$after (must be >=$beforeFirst and <=$last)")
    }

    def unknownNumberProblem(after: Long) = {
      val beforeFirst = queue.headOption.map(_.number - 1) getOrElse torn
      val last = queue.lastOption.map(_.number) getOrElse torn
      Problem.pure(s"Unknown number: Numbered[$vName]: #$after (must be >$beforeFirst and <=$last)")
    }
  }
}

object ObservableNumberedQueue
{
  private val logger = Logger[this.type]
}
