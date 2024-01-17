package js7.base.log

import cats.Applicative
import cats.effect.{IO, Resource, Sync}
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.log.Logger.syntax.*
import org.slf4j.Marker

/** AdHocLogger is used without a logger variable.
 * Not efficient but easy to use. Meant for testing. */
transparent trait AdHocLogger:
  this: Logger.type =>

  inline def error(inline msg: String)(using sourcecode.Enclosing): Unit =
    logger.error(msg)

  inline def warn(inline msg: String)(using sourcecode.Enclosing): Unit =
    logger.warn(msg)

  inline def info(inline msg: String)(using sourcecode.Enclosing): Unit =
    logger.info(msg)

  inline def debug(inline msg: String)(using sourcecode.Enclosing): Unit =
    logger.debug(msg)

  inline def isEnabled(level: LogLevel)(using sourcecode.Enclosing): Boolean =
    logger.isEnabled(level)

  inline def isEnabled(level: LogLevel, marker: Marker)(using sourcecode.Enclosing): Boolean =
    logger.isEnabled(level, marker)

  inline def log(level: LogLevel, message: => String)(using sourcecode.Enclosing): Unit =
    logger.log(level, message)

  inline def log(level: LogLevel, message: => String, throwable: Throwable)
    (using sourcecode.Enclosing)
  : Unit =
    logger.log(level, message, throwable)

  inline def log(level: LogLevel, marker: Marker, message: => String)
    (using sourcecode.Enclosing)
  : Unit =
    logger.log(level, marker, message)

  inline def infoIO[A](body: IO[A])(using sourcecode.Name, sourcecode.Enclosing): IO[A] =
    logger.infoIO(body)

  inline def infoIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.infoIO(functionName, args)(body)

  inline def infoF[F[_], A](body: F[A])(using Sync[F], sourcecode.Name, sourcecode.Enclosing)
  : F[A] =
    logger.infoF(body)

  inline def infoF[F[_], A](functionName: String, args: => Any = "")
    (body: => F[A])
    (using Sync[F], sourcecode.Enclosing)
  : F[A] =
    logger.infoF(functionName, args)(body)

  inline def debugCall[A](body: => A)(using sourcecode.Name, sourcecode.Enclosing): A =
    logger.debugCall(body)

  inline def debugCall[A](functionName: String, args: => Any = "")(body: => A): A =
    logger.debugCall(functionName, args)(body)

  inline def debugIO[A](body: IO[A])(using sourcecode.Name, sourcecode.Enclosing): IO[A] =
    logger.debugIO(body)

  inline def debugIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIO(functionName, args)(body)

  inline def debugF[F[_], A](body: F[A])(using Sync[F], sourcecode.Name, sourcecode.Enclosing)
  : F[A] =
    logger.debugF(body)

  inline def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing)
  : F[A] =
    logger.debugF(functionName, args)(body)

  inline def debugIOWithResult[A](io: IO[A])(using sourcecode.Name, sourcecode.Enclosing): IO[A] =
    logger.debugIOWithResult(io)

  inline def debugIOWithResult[A](function: String)(body: IO[A])(using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIOWithResult(function)(body)

  inline def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
    (using sourcecode.Enclosing): IO[A] =
    logger.debugIOWithResult(function, args)(io)

  inline def debugIOWithResult[A](function: String, args: => Any = "", result: A => Any = null)
    (io: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIOWithResult(function, args, result)(io)

  inline def traceIO[A](body: IO[A])(using sourcecode.Name, sourcecode.Enclosing): IO[A] =
    logger.traceIO(body)

  inline def traceIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing): IO[A] =
    logger.traceIO(functionName, args)(body)

  inline def traceF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
    (using sourcecode.Enclosing): F[A] =
    logger.traceF(body)

  inline def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing)
  : F[A] =
    logger.traceF(functionName, args)(body)

  inline def traceIOWithResult[A](body: IO[A])(using sourcecode.Name, sourcecode.Enclosing): IO[A] =
    logger.traceIOWithResult(body)

  inline def traceIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A](_),
    body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.traceIOWithResult(function, args, result, body)

  inline def traceCall[A](body: => A)(using sourcecode.Name, sourcecode.Enclosing): A =
    logger.traceCall(body)

  inline def traceCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Enclosing)
  : A =
    logger.traceCall(functionName, args)(body)

  inline def traceCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A](_),
    body: => A)
    (using sourcecode.Enclosing)
  : A =
    logger.traceCallWithResult(function, args, result, body)

  inline def infoResource[A](function: String, args: => Any = "")(resource: Resource[IO, A])
    (using sourcecode.Enclosing)
  : Resource[IO, A] =
    logger.infoResource(function, args)(resource)

  inline def debugResource[A](resource: Resource[IO, A])
    (using sourcecode.Name, sourcecode.Enclosing)
  : Resource[IO, A] =
    logger.debugResource(resource)

  inline def debugResource[A](function: String, args: => Any = "")(resource: Resource[IO, A])
    (using sourcecode.Enclosing)
  : Resource[IO, A] =
    logger.debugResource(function, args)(resource)

  inline def traceResource[F[_], A](resource: Resource[F, A])
    (using Applicative[F] & Sync[F], sourcecode.Name, sourcecode.Enclosing)
  : Resource[F, A] =
    logger.traceResource(resource)

  inline def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Applicative[F] & Sync[F], sourcecode.Enclosing)
  : Resource[F, A] =
    logger.traceResource(function, args)(resource)

  inline def infoStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
    (using sourcecode.Enclosing)
  : Stream[IO, A] =
    logger.infoStream(function, args)(stream)

  inline def debugStream[A](stream: Stream[IO, A])(using sourcecode.Name, sourcecode.Enclosing)
  : Stream[IO, A] =
    logger.debugStream(stream)

  inline def debugStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
    (using sourcecode.Enclosing)
  : Stream[IO, A] =
    logger.debugStream(function, args)(stream)

  inline def traceStream[A](stream: Stream[IO, A])
    (using sourcecode.Name, sourcecode.Enclosing, Tag[A])
  : Stream[IO, A] =
    logger.traceStream(stream)

  inline def traceStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
    (using sourcecode.Enclosing)
  : Stream[IO, A] =
    logger.traceStream(function, args)(stream)

  private def logger(using src: sourcecode.Enclosing): ScalaLogger =
    Logger(src.value.replace("#", "."))
