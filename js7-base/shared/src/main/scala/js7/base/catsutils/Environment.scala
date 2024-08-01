package js7.base.catsutils

import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import izumi.reflect.Tag
import java.util.concurrent.ConcurrentHashMap
import js7.base.catsutils.Environment.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import scala.jdk.CollectionConverters.*

/** Maps a type (a Tag) to a value. */
final class Environment:
  private val tagToEntry = new ConcurrentHashMap[Tag[?], Entry]

  /** Add a value of unique type `A` to the `Environment` of our `IORuntime`.
   * <p>
   * Fails if A already has been added.
   */
  private def tryRegister[A](resource: ResourceIO[A])(using tag: Tag[A]): Boolean =
    var added = false
    tagToEntry.computeIfAbsent(tag, tag =>
      added = true
      Entry.Resource(resource))
    added

  @throws[DuplicateEnvironmentTagException]
  private[catsutils] def add[A](a: A)(using tag: Tag[A]): Unit =
    var added = false
    tagToEntry.computeIfAbsent(tag, tag =>
      added = true
      Entry.Pure(a))
    if !added then
      throw DuplicateEnvironmentTagException(tag)

  private def remove[A](a: A)(using tag: Tag[A]): Unit =
    tagToEntry.remove(tag, a)

  /** Get a previously added `A` value from the environment. */
  private def get[A](orRegister: Option[ResourceIO[A]] = None)(using tag: Tag[A]): IO[A] =
    IO.defer:
      val entry = tagToEntry.get(tag)
      entry match
        case null =>
          orRegister match
            case None => IO.raiseError(EnvironmentTagNotFoundException(tag))
            case Some(orRegister) =>
              tagToEntry.replace(tag, entry, Entry.Resource(orRegister))
              get[A]()

        case Entry.Pure(a) =>
          IO.pure(a.asInstanceOf[A])

        case Entry.Alloc(a) =>
          IO.pure(a.allocatedThing.asInstanceOf[A])

        case entry @ Entry.Resource(resource) =>
          logger.debugIO(s"get[${tag.tag}] allocate"):
            resource.toAllocated.flatMap: allocated =>
              val replaced = tagToEntry.replace(tag, entry, Entry.Alloc(allocated))
              IO.unlessA(replaced):
                logger.debugIO(s"get[${tag.tag}] concurrent allocation, release it"):
                  allocated.release
              .as(allocated.allocatedThing.asInstanceOf[A])

  private def release[A](using tag: Tag[A]): IO[Unit] =
    IO.defer:
      tagToEntry.get(tag) match
        case null => IO.unit
        case entry @ Entry.Alloc(a) =>
          logger.traceIO(s"release ${tag.tag}"):
            a.release *> IO(tagToEntry.remove(tag, entry))
        case entry =>
          IO(tagToEntry.remove(tag, entry))

  def releaseAll: IO[Unit] =
    IO.defer:
      tagToEntry.keys.asScala.toVector
        .parTraverse: tag =>
          release(using tag.asInstanceOf[Tag[Any]])
      .map(_.combineAll)


object Environment:

  private lazy val logger = Logger[this.type]

  def register[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[Unit] =
    register2(resource, ignoreDuplicate = false)

  def tryRegister[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[Unit] =
    register2(resource, ignoreDuplicate = true)

  private def register2[A](resource: ResourceIO[A], ignoreDuplicate: Boolean)(using tag: Tag[A])
  : ResourceIO[Unit] =
    Resource:
      OurIORuntimeRegister.environment.flatMap: env =>
        IO:
          val registered = env.tryRegister(resource)
          if !ignoreDuplicate && !registered then
            throw DuplicateEnvironmentTagException(tag)
          else
            () -> IO.whenA(registered)(env.release[A])

  def registerEagerly[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[A] =
    register(resource).evalMap: _ =>
      environment[A]

  def getOrRegister[A](resource: ResourceIO[A])(using tag: Tag[A]): ResourceIO[A] =
    Resource:
      OurIORuntimeRegister.environment.flatMap: env =>
        IO.defer:
          val registered = env.tryRegister(resource)
          environment[A].map: a =>
            a -> IO.whenA(registered)(env.release[A])
  /** Add a value of unique type `A` to the `Environment` of our `IORuntime`.
   * <p>
   * Fails if A already has been added.
   */
  def add[A](a: A)(using tag: Tag[A]): IO[Unit] =
    OurIORuntimeRegister.environment.map(_.add(a))

  ///** Add a value of unique type `A` to the `Environment` of our `IORuntime`.
  // * <p>
  // * Fails if A already has been added.
  // */
  //def add[A](ioRuntime: IORuntime, a: A)(using tag: Tag[A]): Unit =
  //  OurIORuntimeRegister.toEnvironment(ioRuntime).add(a)

  /** Fetches a previously added `A` value from the `Environment` of our `IORuntime`. */
  def environment[A](using tag: Tag[A]): IO[A] =
    OurIORuntimeRegister.environment.flatMap(_.get[A]())

  private enum Entry:
    case Pure(value: Any)
    case Resource(resource: ResourceIO[Any])
    case Alloc(allocated: Allocated[IO, Any])

  private final class EnvironmentTagNotFoundException(val tag: Tag[?])
  extends IllegalArgumentException(s"environment[${tag.tag}] is not defined")

  private final class DuplicateEnvironmentTagException(val tag: Tag[?])
  extends IllegalArgumentException(s"environment[${tag.tag}] has already been defined")
