package js7.base.monixutils

import cats.effect.{IO, Resource, ResourceIO}
import izumi.reflect.Tag
import js7.base.utils.AsyncLock

final class RefCountedResource[A: Tag] private(
  base: ResourceIO[A],
  enclosing: sourcecode.Enclosing):

  private val lock = AsyncLock(s"${enclosing.value}:RefCountedResource[${implicitly[Tag[A]].tag}]")
  @volatile private var maybeCached: Option[Cached] = None

  def resource(using src: sourcecode.Enclosing): ResourceIO[A] =
    resource(src.value)

  def resource(label: String): ResourceIO[A] =
    Resource
      .make(acquire(label))(releaseCached(label))
      .map(_.a)

  private def acquire(label: String): IO[Cached] =
    lock.lock(s"$label->acquire"):
      IO.defer:
        maybeCached match
          case None =>
            base.allocated
              .map: (a, release) =>
                val cached = new Cached(a, release)
                maybeCached = Some(cached)
                cached
          case Some(cached) =>
            cached.refCount += 1
            IO.pure(cached)

  private def releaseCached(label: String)(cached: Cached): IO[Unit] =
    lock.lock(s"$label->releaseCached"):
      IO.defer:
        cached.refCount -= 1
        if cached.releaseOnZero && cached.refCount == 0 then
          cached.release
        else
          IO.unit

  def clear(using src: sourcecode.Enclosing): IO[Unit] =
    lock.lock(src.value + "->clear"):
      IO.defer:
        maybeCached.fold(IO.unit): cached =>
          maybeCached = None
          if cached.refCount == 0 then
            cached.release
          else
            cached.releaseOnZero = true
            IO.unit

  // Maybe race condition with resource.allocated ???
  def release(using src: sourcecode.Enclosing): IO[Unit] =
    lock.lock(src.value + "->release"):
      IO.defer:
        maybeCached match
          case None => IO.unit
          case Some(cached) =>
            maybeCached = None
            cached.release

  def cachedValue: Option[A] =
    maybeCached.map(_.a)

  private class Cached(val a: A, val release: IO[Unit]):
    @volatile var refCount = 1
    @volatile var releaseOnZero = false


object RefCountedResource:

  def apply[A: Tag](base: ResourceIO[A])(using enc: sourcecode.Enclosing): RefCountedResource[A] =
    // Convert implicit Enclosing to an explicit value used by the constructor only
    new RefCountedResource(base, enc)
