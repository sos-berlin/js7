package js7.base.monixutils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.utils.AsyncLock
import monix.eval.Task

final class RefCountedResource[A: Tag](base: Resource[Task, A])
  (implicit enclosing: sourcecode.Enclosing):

  private val lock = AsyncLock(s"${enclosing.value}:RefCountedResource[${implicitly[Tag[A]].tag}]")
  @volatile private var maybeCached: Option[Cached] = None

  def resource(implicit src: sourcecode.Enclosing): Resource[Task, A] =
    resource(src.value)

  def resource(label: String): Resource[Task, A] =
    Resource
      .make(acquire(label))(releaseCached(label))
      .map(_.a)

  private def acquire(label: String): Task[Cached] =
    lock.lock(s"$label->acquire")(Task.defer(
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

  private def releaseCached(label: String)(cached: Cached): Task[Unit] =
    lock.lock(s"$label->releaseCached")(Task.defer {
      cached.refCount -= 1
      if cached.releaseOnZero && cached.refCount == 0 then
        cached.release
      else
        Task.unit
    })

  def clear(implicit src: sourcecode.Enclosing): Task[Unit] =
    lock.lock(src.value + "->clear")(Task.defer {
      maybeCached.fold(Task.unit) { cached =>
        maybeCached = None
        if cached.refCount == 0 then
          cached.release
        else {
          cached.releaseOnZero = true
          Task.unit
        }
      }
    })

  // Maybe race condition with resource.allocated ???
  def release(implicit src: sourcecode.Enclosing): Task[Unit] =
    lock.lock(src.value + "->release")(Task.defer(
      maybeCached match {
        case None => Task.unit
        case Some(cached) =>
          maybeCached = None
          cached.release
      }))

  def cachedValue: Option[A] =
    maybeCached.map(_.a)

  private class Cached(val a: A, val release: Task[Unit]):
    @volatile var refCount = 1
    @volatile var releaseOnZero = false
