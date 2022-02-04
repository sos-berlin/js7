package js7.base.stream

import izumi.reflect.Tag
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.execution.Ack
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject
import scala.util.{Failure, Success}

final class ObservableNumberedQueue[V: Tag]
{
  private val vName = implicitly[Tag[V]].tag.toString
  private var torn = 0L
  private var queue = Vector.empty[Numbered[V]]
  private val sync = new IncreasingNumberSync(initial = 0, i => s"#$i")
  private val lock = AsyncLock("ObservableNumberedQueue", suppressLog = true)

  def enqueue(commands: Iterable[V]): Task[Unit] =
    lock.lock(Task {
      var lastNumber: Option[Long] = None
      for (command <- commands) {
        val number = queue.lastOption.fold(torn)(_.number) + 1
        queue = queue :+ Numbered(number, command)
        lastNumber = Some(number)
      }
      lastNumber foreach sync.onAdded
    })

  def observable(after: Long): Observable[Seq[Numbered[V]]] =
    Observable.deferAction { implicit s =>
      import Ack.{Continue, Stop}
      val subject = PublishToOneSubject[Seq[Numbered[V]]]()
      var ack = subject.subscription

      def loop(after: Long): Unit =
        ack.syncOnContinue {
          checkNumber(after)
            .*>(sync.whenAvailable(after = after, until = None, delay = 0.s))
            .*>(readQueue(after))
            .materialize
            .foreach {
              case Failure(t) => subject.onError(t)
              case Success(values) =>
                assertThat(values.nonEmpty)
                ack = ack.syncFlatMap {
                  case Stop => Stop
                  case Continue => subject.onNext(values)
                }
                loop(after = values.lastOption.fold(after)(_.number))
            }
        }

      loop(after)
      subject
    }

  private def readQueue(after: Long): Task[Vector[Numbered[V]]] =
    lock.lock(Task.defer {
      val (index, found) = binarySearch(0, queue.length, queue(_).number.compare(after))
      if (!found && after != torn)
        Task.raiseError(unknownNumber(after).throwable)
      else
        Task.pure(queue.drop(index + found.toInt))
    })

  private def checkNumber(after: Long): Task[Unit] =
    Task.defer {
      Task.when(after < torn || queue.lastOption.map(_.number).exists(_ < after))(
        Task.raiseError(unknownNumber(after).throwable))
    }

  private def unknownNumber(after: Long) =
    Problem.pure(s"Unknown Numbered[$vName]: #$after")

  def releaseUntil(after: Long): Task[Checked[Unit]] =
    lock.lock(Task {
      if (after < torn)
        Left(Problem.pure(s"releaseUntil($after) < torn=$torn ?"))
      else if (after > torn && queue.lastOption.forall(_.number < after))
        Left(Problem.pure(s"releaseUntil($after) > last.number ?"))
      else {
        torn = after
        val index = binarySearch(0, queue.length, queue(_).number.compare(after))._1
        queue = queue.drop(index)
        Checked.unit
      }
    })

  override def toString = s"ObservableNumberedQueue[$vName]"
}
