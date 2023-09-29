package js7.base.monixutils

import cats.effect.Resource
import monix.catnap.MVar
import monix.eval.Task

final class SimpleLock:
  private val lockV = MVar[Task].of(()).memoize

  val resource = Resource.make(
    acquire = lockV.flatMap(_.take))(
    release = _ => lockV.flatMap(_.tryPut(()).map {
      case false => sys.error("Switch: lockV release failed")
      case true => ()
    }))

  def lock[A](task: Task[A]): Task[A] =
    resource.use(_ => task)
