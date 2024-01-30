package js7.base.log

import cats.effect.{IO, Resource}
import fs2.Stream
import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag
import sourcecode.{FileName, Line, Name, Pkg}

object Logger
{
  val empty: scribe.Logger =
    scribe.Logger.empty

  def initialize() = {}

  def apply[A: ClassTag]: scribe.Logger =
    apply(implicitClass[A])

  def apply(c: Class[?]): scribe.Logger =
    scribe.Logger(normalizeClassName(c))

  def apply(name: String): scribe.Logger =
    scribe.Logger(name)

  /** Removes '$' from Scala's companion object class. */
  def normalizeClassName(c: Class[?]): String =
    c.getName stripSuffix "$"

  object syntax {
    // Empty implementation to be compatible with the JVM variant
    implicit final class RichLogger(private val logger: scribe.Logger) extends AnyVal {
      def isEnabled(level: LogLevel): Boolean =
        level != LogLevel.LogNone && logger.includes(logLevelToScribe(level))

      def log(level: LogLevel, message: => String)
        (implicit pkg: Pkg, fileName: FileName, name: Name, line: Line)
      : Unit =
        if (level != LogLevel.LogNone) {
          logger.log(logLevelToScribe(level), message, throwable = None)
        }

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        if (level != LogLevel.LogNone) {
          logger.log(logLevelToScribe(level), message, Some(throwable))
        }

      inline def debugIO[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def debugIO[A](functionName: String, inline args: => Any = "")(io: IO[A]): IO[A] =
        io

      inline def debugIOWithResult[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def debugIOWithResult[A](
        inline function: String,
        inline args: => Any = "",
        inline result: A => Any = identity[A](_))
        (io: IO[A])
      : IO[A] =
        io

      inline def traceIO[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def traceIO[A](function: String, inline args: => Any = "")(io: IO[A]): IO[A] =
        io

      inline def traceIOWithResult[A](io: IO[A]): IO[A] =
        io //traceIOWithResult[A](src.value)(io)

      inline def traceIOWithResult[A](
        inline function: String,
        inline args: => Any = "",
        inline result: A => Any = identity[A](_))
        (io: IO[A])
      : IO[A] =
        io

      inline def debugResource[A](resource: Resource[IO, A])
      : Resource[IO, A] =
        resource //debugResource(src.value)(resource)

      inline def debugResource[A](function: String, inline args: => Any = "")
        (resource: Resource[IO, A])
      : Resource[IO, A] =
        resource

      inline def debugStream[A](stream: Stream[IO, A]): Stream[IO, A] =
        stream

      inline def debugStream[A](inline function: String, inline args: => Any = "")
        (stream: Stream[IO, A])
      : Stream[IO, A] =
        stream

      inline def traceStream[A](stream: Stream[IO, A]): Stream[IO, A] =
        stream

      inline def traceStream[A](function: String, inline args: => Any = "")(stream: Stream[IO, A])
      : Stream[IO, A] =
        stream
    }
  }

  private def logLevelToScribe(logLevel: LogLevel): scribe.Level =
    logLevel match {
      case LogLevel.LogNone => null
      case LogLevel.Trace => scribe.Level.Trace
      case LogLevel.Debug => scribe.Level.Debug
      case LogLevel.Info => scribe.Level.Info
      case LogLevel.Warn => scribe.Level.Warn
      case LogLevel.Error => scribe.Level.Error
    }
}
