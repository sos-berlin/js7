package js7.base.catsutils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import izumi.reflect.Tag
import java.util.concurrent.ConcurrentHashMap
import js7.base.catsutils.Environment.*
import js7.base.log.Logger

/** Maps a type (a Tag) to a value. */
final class Environment:
  private val tagToValue = new ConcurrentHashMap[Tag[?], Entry]

  @throws[IllegalArgumentException]
  def add[A](a: A)(using tag: Tag[A]): Unit =
    var added = false
    tagToValue.computeIfAbsent(tag, tag =>
      added = true
      Entry.Pure(a))
    if !added then
      throw new IllegalArgumentException(s"environment[${tag.tag}] has already been defined")

  /** Get a previously added `A` value from the environment. */
  @throws[IllegalArgumentException]
  def get[A](using tag: Tag[A]): IO[A] =
    tagToValue.get(tag) match
      case null =>
        throw new IllegalArgumentException(s"No environment[${tag.tag}]")
      case Entry.Pure(a) =>
        IO.pure(a.asInstanceOf[A])
      //case Entry.Delay(io) =>
      //  io.map(_.asInstanceOf[A])


object Environment:
  private val logger = Logger[this.type]

  /** Add a value of unique type `A` to the `Environment` of our `IORuntime`.
   * <p>
   * Fails if A already has been added.
   */
  def add[A](a: A)(using tag: Tag[A]): IO[Unit] =
    OurIORuntimeRegister.environment.map(_.add(a))

  /** Fetches a previously added `A` value from the `Environment` of our `IORuntime`. */
  def environment[A](using tag: Tag[A]): IO[A] =
    OurIORuntimeRegister.environment.flatMap(_.get[A])

  extension (ioRuntime: IORuntime)
    def environment: Environment =
      OurIORuntimeRegister.toEnvironment(ioRuntime)

  enum Entry:
    case Pure(any: Any)
    //case Delay(io: IO[Any])
    //case Alloc(allocated: Allocated[IO, Any])
