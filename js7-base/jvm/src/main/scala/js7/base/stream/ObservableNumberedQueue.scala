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
  private var torn = 0L
  private var nextNumber = 1L
  @volatile private var queue = Vector.empty[Numbered[V]]
  private val sync = new IncreasingNumberSync(initial = 0, i => s"#$i")
  private val lock = AsyncLock("ObservableNumberedQueue", suppressLog = true)

  def enqueue(commands: Iterable[V]): Task[Unit] =
    lock.lock(
      Task {
        var lastNumber = -1L
        for (command <- commands) {
          queue :+= Numbered(nextNumber, command)
          lastNumber = nextNumber
          nextNumber += 1
        }
        lastNumber
      }.flatMap(syncNumber)) // This separate Task should work as a write barrier for queue (?)

  def enqueueNumbered(commands: Iterable[Numbered[V]]): Task[Unit] =
    Task.when(commands.nonEmpty)(
      lock.lock(Task {
        var lastNumber = -1L
        for (command <- commands) {
          assertThat(command.number >= nextNumber)
          queue :+= command
          lastNumber = command.number
          nextNumber = command.number + 1
        }
        lastNumber
      }.flatMap(syncNumber))) // This separate Task should work as a write barrier for queue (?)

  private def syncNumber(lastNumber: Long): Task[Unit] =
    Task {
      if (lastNumber != -1) sync.onAdded(lastNumber)
    }

  def observable: Observable[Seq[Numbered[V]]] =
    observable(torn)

  def observable(after: Long): Observable[Seq[Numbered[V]]] =
    Observable.deferAction { implicit s =>
      val subject = PublishToOneSubject[Seq[Numbered[V]]]()

      def loop(ack: Future[Ack], after: Long): Unit =
        ack.syncTryFlatten.syncOnContinue {
          checkNumber(after)
            // Should we delay some milliseconds to reduce number of command batches ???
            .*>(sync.whenAvailable(after = after, until = None, delay = 0.s))
            .*>(readQueue(after))
            .runToFuture  // Be sure to leave the `loop` recursion stack
            .onComplete {
              case Failure(t) =>
                subject.onError(t)

              case Success(values) =>
                if (values.isEmpty) {
                  // Race condition ???
                  logger.error(
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

  private def readQueue(after: Long): Task[Vector[Numbered[V]]] =
    lock.lock(Task.defer {
      val q = queue
      val (index, found) = binarySearch(0, q.length, q(_).number.compare(after))
      if (!found && after != torn)
        Task.raiseError(unknownNumberProblem(after).throwable)
      else
        Task.pure(q.drop(index + found.toInt))
    })

  private def checkNumber(after: Long): Task[Unit] =
    Task.defer {
      Task.when(after < torn || queue.lastOption.map(_.number).exists(_ < after))(
        Task.raiseError(unknownNumberProblem(after).throwable))
    }

  private def unknownNumberProblem(after: Long) =
    Problem.pure(s"Unknown Numbered[$vName]: #$after")

  def releaseUntil(after: Long): Task[Checked[Unit]] =
    lock.lock(Task {
      val q = queue
      if (after < torn)
        Left(Problem.pure(s"releaseUntil($after) < torn=$torn ?"))
      else if (after > torn && q.lastOption.exists(_.number < after))
        Left(Problem.pure(
          s"releaseUntil($after) > ${q.lastOption.map(_.number).getOrElse("None")} ?"))
      else {
        torn = after
        val index = binarySearch(0, q.length, q(_).number.compare(after))._1
        queue = q.drop(index)
        Checked.unit
      }
    })

  def dequeueAll: Task[Vector[Numbered[V]]] =
    lock.lock(Task {
      val result = queue
      queue = Vector.empty
      result
    })

  override def toString = s"ObservableNumberedQueue[$vName]"
}

object ObservableNumberedQueue
{
  private val logger = Logger[this.type]
}
