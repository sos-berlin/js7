package js7.base.stream

import izumi.reflect.Tag
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.AsyncLock
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.reactive.Observable

final class ObservableNumberedQueue[V: Tag]
{
  private var torn = 0L
  private var queue = Vector.empty[Numbered[V]]
  private val sync = new IncreasingNumberSync(initial = 0, i => s"#$i")
  private val lock = AsyncLock()

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

  def observable(after: Long): Task[Checked[Observable[Numbered[V]]]] =
    lock
      .lock(Task {
        val (index, found) = binarySearch(0, queue.length, queue(_).number.compare(after))
        if (!found && after != torn)
          Left(Problem.pure(s"Unknown Numbered[$vName]: #$after"))
        else
          Right(queue.drop(index + found.toInt))
      })
      .map(_.map { queued =>
        @volatile var lastNumber = after
        Observable
          .fromIterable(queued)
          .map { o =>
            lastNumber = o.number
            o
          } ++
          Observable
            .fromTask(
              sync.whenAvailable(after = lastNumber, until = None, delay = 0.s))
            .flatMap(_ => Observable.fromTask(
              observable(lastNumber)
                .map(_ getOrElse Observable.empty)))
            .flatten
        })

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

  override def toString =
    s"ObservableNumberedQueue[$vName]"

  private def vName = implicitly[Tag[V]].tag.toString
}
