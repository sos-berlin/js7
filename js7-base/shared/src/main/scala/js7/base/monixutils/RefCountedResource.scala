package js7.base.monixutils

import cats.effect.Resource
import js7.base.utils.TaskLock
import monix.eval.Task

final class RefCountedResource[A](base: Resource[Task, A])
{
  private val lock = TaskLock("RefCountedResource")
  @volatile private var maybeCached: Option[Cached] = None

  val resource: Resource[Task, A] =
    Resource
      .make(acquire)(releaseCached)
      .map(_.a)

  private def acquire: Task[Cached] =
    lock.lock(Task.defer(
      maybeCached match {
        case None =>
          base.allocated
            .map { case (a, release) =>
              val cached = new Cached(a, release)
              maybeCached = Some(cached)
              cached
            }
        case Some(cached) =>
          cached.refCount += 1
          Task.pure(cached)
      }))

  private def releaseCached(cached: Cached): Task[Unit] =
    lock.lock(Task.defer {
      cached.refCount -= 1
      if (cached.releaseOnZero && cached.refCount == 0)
        cached.release
      else
        Task.unit
    })

  def clear: Task[Unit] =
    lock.lock(Task.defer {
      maybeCached.fold(Task.unit) { cached =>
        maybeCached = None
        if (cached.refCount == 0)
          cached.release
        else {
          cached.releaseOnZero = true
          Task.unit
        }
      }
    })

  // Maybe race condition with resource.allocated ???
  def release: Task[Unit] =
    lock.lock(Task.defer(
      maybeCached match {
        case None => Task.unit
        case Some(cached) =>
          maybeCached = None
          cached.release
      }))

  private class Cached(val a: A, val release: Task[Unit]) {
    @volatile var refCount = 1
    @volatile var releaseOnZero = false
  }
}
