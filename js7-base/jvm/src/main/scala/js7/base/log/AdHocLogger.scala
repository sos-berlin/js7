package js7.base.log

import cats.effect.{IO, Resource, Sync}
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.log.Logger.syntax.*
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.Tests.isTest
import org.slf4j.Marker

/** AdHocLogger is used without a logger variable.
 * Not efficient but easy to use. Meant for testing. */
transparent trait AdHocLogger:
  this: Logger.type =>

  inline def error(inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.error(withLoc(msg))

  inline def error(inline msg: String, inline throwable: Throwable)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.error(withLoc(msg), throwable)

  inline def warn(inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.warn(withLoc(msg))

  inline def info(inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.info(withLoc(msg))

  inline def debug(inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.debug(withLoc(msg))

  inline def trace(inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.trace(withLoc(msg))

  inline def isEnabled(level: LogLevel)
    (using inline enc: sourcecode.Enclosing)
  : Boolean =
    logger.isEnabled(level)

  inline def isEnabled(level: LogLevel, marker: Marker)
    (using inline enc: sourcecode.Enclosing)
  : Boolean =
    logger.isEnabled(level, marker)

  inline def log(level: LogLevel, inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.log(level, withLoc(msg))

  inline def log(level: LogLevel, inline msg: String, throwable: Throwable)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.log(level, withLoc(msg), throwable)

  inline def log(level: LogLevel, marker: Marker, inline msg: String)
    (using inline enc: sourcecode.Enclosing, inline loc: ScalaSourceLocation)
  : Unit =
    logger.log(level, marker, withLoc(msg))

  inline def infoCall[A](inline body: => A)(using inline src: sourcecode.Name): A =
    logger.infoCall[A](body)

  inline def infoCall[A](inline functionName: String, inline args: => Any = "")(inline body: => A)
  : A =
    logger.infoCall(functionName, args)(body)

  inline def infoCallWithResult[A](inline body: => A)(using inline src: sourcecode.Name): A =
    logger.infoCallWithResult(body)

  inline def infoCallWithResult[A](inline function: String)(inline body: => A): A =
    logger.infoCallWithResult(function)(body)

  inline def infoCallWithResult[A](inline function: String, inline args: => Any)
    (inline body: => A): A =
    logger.infoCallWithResult(function, args)(body)

  inline def infoCallWithResult[A](
    inline function: String,
    inline args: => Any = "",
    inline result: A => Any = identity[A],
    inline marker: Marker | Null = null,
    inline body: => A)
    (using inline enc: sourcecode.Enclosing)
  : A =
    logger.infoCallWithResult(function, args, result, marker, body)

  inline def infoIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using inline enc: sourcecode.Enclosing)
  : IO[A] =
    logger.infoIO(functionName, args)(body)

  inline def infoIO[A](body: IO[A])
    (using inline enc: sourcecode.Enclosing, nam: sourcecode.Name)
  : IO[A] =
    logger.infoIO(body)

  inline def infoF[F[_], A](body: F[A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.infoF(body)

  inline def infoF[F[_], A](functionName: String, args: => Any = "")
    (body: => F[A])
    (using Sync[F], sourcecode.Enclosing)
  : F[A] =
    logger.infoF(functionName, args)(body)

  def infoIOWithResult[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.infoIOWithResult(body)

  def infoIOWithResult[A](function: String)(body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.infoIOWithResult(function)(body)

  def infoIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.infoIOWithResult(function, args, result, marker, body)

  inline def debugCall[A](body: => A)(using sourcecode.Enclosing, sourcecode.Name): A =
    logger.debugCall(body)

  inline def debugCall[A](functionName: String, args: => Any = "")(body: => A): A =
    logger.debugCall(functionName, args)(body)

  inline def debugIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIO(body)

  inline def debugIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIO(functionName, args)(body)

  inline def debugF[F[_], A](body: F[A])(using Sync[F], sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.debugF(body)

  inline def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F],sourcecode.Enclosing)
  : F[A] =
    logger.debugF(functionName, args)(body)

  inline def debugIOWithResult[A](io: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIOWithResult(io)

  inline def debugIOWithResult[A](function: String)(body: IO[A])(using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIOWithResult(function)(body)

  inline def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
    (using sourcecode.Enclosing): IO[A] =
    logger.debugIOWithResult(function, args)(io)

  inline def debugIOWithResult[A](
    function: String,
    args: => Any = "",
    result: (A => Any) | Null = null)
    (io: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.debugIOWithResult(function, args, result)(io)

  inline def traceIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.traceIO(body)

  inline def traceIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing): IO[A] =
    logger.traceIO(functionName, args)(body)

  inline def traceF[F[_], A](body: F[A])(using F: Sync[F])
    (using sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.traceF(body)

  inline def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing)
  : F[A] =
    logger.traceF(functionName, args)(body)

  inline def traceIOWithResult[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.traceIOWithResult(body)

  inline def traceIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing)
  : IO[A] =
    logger.traceIOWithResult(function, args, result, marker, body)

  inline def traceCall[A](body: => A)(using sourcecode.Enclosing, sourcecode.Name): A =
    logger.traceCall(body)

  inline def traceCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Enclosing)
  : A =
    logger.traceCall(functionName, args)(body)

  inline def traceCallWithResult[A](function: String)(body: => A)(using sourcecode.Enclosing): A =
    logger.traceCallWithResult(function)(body)

  inline def traceCallWithResult[A](function: String, args: => Any)(body: => A)
    (using sourcecode.Enclosing)
  : A =
    logger.traceCallWithResult(function, args)(body)

  inline def traceCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    body: => A)
    (using sourcecode.Enclosing)
  : A =
    logger.traceCallWithResult(function, args, result, body)

  def infoResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing)
  : Resource[F, A] =
    logger.infoResource[F, A](function, args)(resource)

  def infoResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.infoResource[F, A](resource)

  def debugResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.debugResource[F, A](resource)

  def debugResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing)
  : Resource[F, A] =
    logger.debugResource[F, A](function, args)(resource)

  def traceResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.debugResource[F, A](resource)

  def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F])
  : Resource[F, A] =
    logger.traceResource[F, A](function, args)(resource)

  def infoStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F])
  : Stream[F, A] =
    logger.infoStream[F, A](function, args)(stream)

  def debugStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Stream[F, A] =
    logger.debugStream[F, A](stream)

  def debugStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F])
  : Stream[F, A] =
    logger.debugStream[F, A](function, args)(stream)

  def traceStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Name, Tag[A])
  : Stream[F, A] =
    logger.traceStream[F, A](stream)

  def traceStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing)
  : Stream[F, A] =
    logger.traceStream[F, A](function, args)(stream)

  private val nameRegex = """#\$.*$""".r

  private def logger(using src: sourcecode.Enclosing): ScalaLogger =
    Logger(nameRegex.replaceFirstIn(src.value, "").replace("#", "."))

  private inline def withLoc(inline msg: String)(using inline loc: ScalaSourceLocation): String =
    s"$loc ${if isTest then "🔺 " else ""}$msg"
