package js7.base.monixutils

import cats.effect.{IO, Resource}
import izumi.reflect.Tag
import js7.base.utils.AsyncLock

final class RefCountedResource[A: Tag](base: Resource[IO, A])
  (implicit enclosing: sourcecode.Enclosing):

  private val lock = AsyncLock(s"${enclosing.value}:RefCountedResource[${implicitly[Tag[A]].tag}]")
  @volatile private var maybeCached: Option[Cached] = None

  def resource(implicit src: sourcecode.Enclosing): Resource[IO, A] =
    resource(src.value)

  def resource(label: String): Resource[IO, A] =
    Resource
      .make(acquire(label))(releaseCached(label))
      .map(_.a)

  private def acquire(label: String): IO[Cached] =
    lock.lock(s"$label->acquire")(IO.defer(
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
          IO.pure(cached)
      }))

  private def releaseCached(label: String)(cached: Cached): IO[Unit] =
    lock.lock(s"$label->releaseCached")(IO.defer {
      cached.refCount -= 1
      if cached.releaseOnZero && cached.refCount == 0 then
        cached.release
      else
        IO.unit
    })

  def clear(implicit src: sourcecode.Enclosing): IO[Unit] =
    lock.lock(src.value + "->clear")(IO.defer {
      maybeCached.fold(IO.unit) { cached =>
        maybeCached = None
        if cached.refCount == 0 then
          cached.release
        else {
          cached.releaseOnZero = true
          IO.unit
        }
      }
    })

  // Maybe race condition with resource.allocated ???
  def release(implicit src: sourcecode.Enclosing): IO[Unit] =
    lock.lock(src.value + "->release")(IO.defer(
      maybeCached match {
        case None => IO.unit
        case Some(cached) =>
          maybeCached = None
          cached.release
      }))

  def cachedValue: Option[A] =
    maybeCached.map(_.a)

  private class Cached(val a: A, val release: IO[Unit]):
    @volatile var refCount = 1
    @volatile var releaseOnZero = false
