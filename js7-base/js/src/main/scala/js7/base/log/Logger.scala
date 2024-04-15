package js7.base.log

import cats.effect.kernel.Sync
import cats.effect.{IO, Resource, ResourceIO}
import fs2.Stream
import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag
import sourcecode.{FileName, Line, Name, Pkg}

object Logger
{
  type Underlying = scribe.Logger

  val empty: scribe.Logger =
    scribe.Logger.empty

  def initialize(name: String) = {}

  def isInitialized: Boolean = true

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
    extension (logger: scribe.Logger)
      /** Can be used to import these extensions. */
      def forceImportExtensions: Unit = ()

      def isEnabled(level: LogLevel): Boolean =
        level != LogLevel.LogNone && logger.includes(logLevelToScribe(level))

      inline def whenTraceEnabled(inline body: => Unit): Unit =
        if logger.includes(scribe.Level.Trace) then
          body

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

      inline def infoIO[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def infoIO[A](functionName: String, inline args: => Any = "")(io: IO[A]): IO[A] =
        io

      def infoF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        infoF[F, A](functionName = src.value)(body)

      def infoF[F[_], A](functionName: String, args: => Any = "")
        (body: F[A])
        (using F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Info, functionName, args)(body)

      inline def debugIO[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def debugIO[A](functionName: String, inline args: => Any = "")(io: IO[A]): IO[A] =
        io

      inline def debugIOWithResult[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def debugIOWithResult[A](
        inline function: String,
        inline args: => Any = "",
        inline result: A => Any = identity[A](_),
        inline body: IO[A])
      : IO[A] =
        body

      def debugF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        debugF[F, A](functionName = src.value)(body)

      def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Debug, functionName, args)(body)

      inline def traceIO[A](io: IO[A])/*(implicit src: sourcecode.Name)*/: IO[A] =
        io

      inline def traceIO[A](function: String, inline args: => Any = "")(io: IO[A]): IO[A] =
        io

      inline def traceIOWithResult[A](io: IO[A]): IO[A] =
        io //traceIOWithResult[A](src.value)(io)

      inline def traceIOWithResult[A](
        inline function: String,
        inline args: => Any = "",
        inline result: A => Any = identity[A](_),
        inline body: IO[A])
      : IO[A] =
        body

      def traceF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        traceF(functionName = src.value)(body)

      def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Trace, functionName, args)(body)

      inline def debugResource[A](resource: ResourceIO[A])
      : ResourceIO[A] =
        resource //debugResource(src.value)(resource)

      inline def debugResource[A](function: String, inline args: => Any = "")
        (resource: ResourceIO[A])
      : ResourceIO[A] =
        resource

      inline def traceResource[A](resource: ResourceIO[A])
      : ResourceIO[A] =
        resource //traceResource(src.value)(resource)

      inline def traceResource[A](function: String, inline args: => Any = "")
        (resource: ResourceIO[A])
      : ResourceIO[A] =
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

  private def logF[F[_], A](
    logger: Underlying,
    logLevel: LogLevel,
    function: String,
    args: => Any = "",
    resultToLoggable: A => Any = null)
    (body: F[A])
    (implicit F: Sync[F])
  : F[A] =
    body

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
