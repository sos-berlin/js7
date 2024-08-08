package js7.base.catsutils

import cats.effect.{IO, Resource, ResourceIO, Sync, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import izumi.reflect.Tag
import java.util.concurrent.ConcurrentHashMap
import js7.base.catsutils.Environment.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass

/** Maps a type (a Tag) to a value. */
final class Environment:
  private val tagToEntry = new ConcurrentHashMap[Tag[?], Entry[?]]

  def register[F[_], A](resource: Resource[F, A], ignoreDuplicate: Boolean = false)
    (using F: Sync[F], tag: Tag[A])
  : Resource[F, Unit] =
    Resource:
      F.delay:
        val registered = tryRegister(resource)
        if !registered && !ignoreDuplicate then
          throw DuplicateEnvironmentTagException(tag)
        else
          () -> F.whenA(registered)(release[F, A])

  /** Add a Resource of unique type `A` to the `Environment`.
   * <p>
   * @return false, iff A type has already been registered
   */
  private def tryRegister[F[_], A](resource: Resource[F, A])(using F: Sync[F], tag: Tag[A])
  : Boolean =
    if F.ne(IOSync) && F.ne(SyncIOSync) then throw IllegalArgumentException:
      s"Environment#tryRegister[${F.getClass.simpleScalaName}[${tag.tag}]]: Must be IO[_] or SyncIO[_]"
    var added = false
    tagToEntry.computeIfAbsent(tag, _ =>
      added = true
      Entry.FResource(resource))
    added

  //def releaseAll: IO[Unit] =
  //  IO.defer:
  //    tagToEntry.keys.asScala.toVector
  //      .parTraverse: tag =>
  //        given Tag[?] = tag
  //        release[IO, Any]
  //      .map(_.combineAll)

  private def release[F[_], A](using F: Sync[F], tag: Tag[A]): F[Unit] =
    F.defer:
      tagToEntry.remove(tag) match
        case alloc: Entry.FAlloc[F @unchecked, A @unchecked] =>
          logger.traceF(s"release ${tag.tag}"):
            alloc.release
        case _ =>
          F.unit

  /** Get a previously added `A` value from the environment. */
  private def get[A](orRegister: Option[ResourceIO[A]] = None)(using tag: Tag[A]): IO[A] =
    IO.defer:
      val entry = tagToEntry.get(tag)
      entry match
        case null =>
          orRegister match
            case None => IO.raiseError(EnvironmentTagNotFoundException(tag))
            case Some(resource) =>
              tagToEntry.replace(tag, entry, Entry.FResource(resource))
              get[A](orRegister)

        case pure: Entry.Pure[A @unchecked] =>
          IO.pure(pure.a)

        case alloc: Entry.FAlloc[_, A @unchecked] =>
          IO.pure(alloc.a)

        case entry: Entry.FResource[_, A @unchecked] =>
          import entry.F // F is Sync[IO] or Sync[SyncIO]
          logger.debugIO(s"get[${tag.tag}] allocate"):
            toIO:
              entry.resource.allocated.flatMap: allocated =>
                val (a, release) = allocated
                val replaced = tagToEntry.replace(tag, entry, Entry.FAlloc(allocated))
                if !replaced then
                  logger.debugF(s"get[${tag.tag}] concurrent duplicate allocation, release it"):
                    release.as(None)
                else
                  F.pure(Some(a))
            .flatMap:
              case None => get[A](orRegister)
              case Some(a) => IO.pure(a)


object Environment:

  private lazy val logger = Logger[this.type]

  /** Register a Resource which is allocated and its value is returned immediately. */
  //def registerEagerly[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[A] =
  //  register(resource).evalMap: _ =>
  //    environment[A]

  /** Register a Resource for lazy allocation. */
  def register[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[Unit] =
    register2(resource, ignoreDuplicate = false)

  /** Try to (duplicate) register a Resource for lazy allocation. */
  def tryRegister[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[Unit] =
    register2(resource, ignoreDuplicate = true)

  private def register2[A](resource: ResourceIO[A], ignoreDuplicate: Boolean)(using tag: Tag[A])
  : ResourceIO[Unit] =
    Resource.eval(OurIORuntimeRegister.environment).flatMap: env =>
      env.register(resource, ignoreDuplicate)

  /** Return the value of a registered resource, register the Resource when needed. */
  def getOrRegister[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[A] =
    Resource:
      OurIORuntimeRegister.environment.flatMap: env =>
        IO.defer:
          val registered = env.tryRegister(resource)
          environment[A].map: a =>
            a -> IO.whenA(registered)(env.release[IO, A])

  /** Fetches a registered `A` value from the `Environment` of our `IORuntime`. */
  def environment[A](using tag: Tag[A]): IO[A] =
    OurIORuntimeRegister.environment.flatMap(_.get[A]())

  private val IOSync: Sync[IO] = summon[Sync[IO]]
  private val SyncIOSync: Sync[SyncIO] = summon[Sync[SyncIO]]
  private val AllowedSyncs = Set(IOSync, SyncIOSync)

  private def toIO[F[_], A](fa: F[A])(using F: Sync[F]): IO[A] =
    if F eq SyncIOSync.asInstanceOf[Sync[F]] then
      fa.asInstanceOf[SyncIO[A]].to[IO]
    else if F eq IOSync.asInstanceOf[Sync[F]] then
      fa.asInstanceOf[IO[A]]
    else
      IO.raiseError(RuntimeException:
        s"Environment: Expected IO or SyncIO, but not: ${F.getClass.simpleScalaName}[_]")


  private sealed trait Entry[A]
  private object Entry:
    final case class Pure[A](a: A)
    extends Entry[A]

    final case class FResource[Fa[_], A](
      resource: Resource[Fa, A])(
      using val F: Sync[Fa],
      tag: Tag[A])
    extends Entry[A]:
      type F[x] = Fa[x]
      given Tag[A] = tag

    final case class FAlloc[F[_], A](allocated: (A, F[Unit]))(using val F: Sync[F])
    extends Entry[A]:
      def a: A = allocated._1
      def release: F[Unit] = allocated._2

  private final class EnvironmentTagNotFoundException(val tag: Tag[?])
  extends IllegalArgumentException(s"environment[${tag.tag}] is not defined")

  private final class DuplicateEnvironmentTagException(val tag: Tag[?])
  extends IllegalArgumentException(s"environment[${tag.tag}] has already been defined")
