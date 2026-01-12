package js7.base.log

import cats.effect.{IO, Resource, Sync}
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.log.Logger.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import org.slf4j.Marker

/** AdHocLogger is used without a logger variable.
 * Not efficient but easy to use. Meant for testing. */
transparent trait AdHocLogger:
  this: Logger.type =>

  def error(msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.error(withLoc(msg))

  def error(msg: String, throwable: Throwable)(using sourcecode.Enclosing, sourcecode.Line)
  : Unit =
    logger.error(withLoc(msg), throwable)

  def warn(msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.warn(withLoc(msg))

  def warn(msg: String, throwable: Throwable)(using sourcecode.Enclosing, sourcecode.Line)
  : Unit =
    logger.warn(withLoc(msg), throwable)

  def info(msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.info(withLoc(msg))

  def debug(msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.debug(withLoc(msg))

  def trace(msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.trace(withLoc(msg))

  inline def isEnabled(level: LogLevel): Boolean =
    logger.isEnabled(level)

  inline def isEnabled(level: LogLevel, marker: Marker): Boolean =
    logger.isEnabled(level, marker)

  def log(level: LogLevel, msg: String)(using sourcecode.Enclosing, sourcecode.Line): Unit =
    logger.log(level, withLoc(msg))

  def log(level: LogLevel, msg: String, throwable: Throwable)
    (using enc: sourcecode.Enclosing, loc: sourcecode.Line)
  : Unit =
    logger.log(level, withLoc(msg), throwable)

  def log(level: LogLevel, marker: Marker, msg: String)
    (using sourcecode.Enclosing, sourcecode.Line)
  : Unit =
    logger.log(level, marker, withLoc(msg))

  inline def infoCall[A](inline body: => A)(using inline src: sourcecode.Name): A =
    logger.infoCall[A](body)

  def infoCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Line): A =
    logger.infoCall(withLoc(functionName), args)(body)

  inline def infoCallWithResult[A](inline body: => A)(using inline src: sourcecode.Name): A =
    logger.infoCallWithResult(body)

  def infoCallWithResult[A](function: String)(body: => A)(using sourcecode.Line): A =
    logger.infoCallWithResult(withLoc(function))(body)

  def infoCallWithResult[A](function: String, args: => Any)(body: => A)(using sourcecode.Line)
  : A =
    logger.infoCallWithResult(withLoc(function), args)(body)

  def infoCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: => A)
    (using sourcecode.Enclosing, sourcecode.Line)
  : A =
    logger.infoCallWithResult(withLoc(function), args, result, marker, body)

  def infoIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.infoIO(withLoc(functionName), args)(body)

  inline def infoIO[A](body: IO[A])
    (using inline enc: sourcecode.Enclosing, nam: sourcecode.Name)
  : IO[A] =
    logger.infoIO(body)

  inline def infoF[F[_], A](body: F[A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.infoF(body)

  def infoF[F[_], A](functionName: String, args: => Any = "")
    (body: => F[A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : F[A] =
    logger.infoF(withLoc(functionName), args)(body)

  inline def infoIOWithResult[A](inline body: IO[A])
    (using inline enc: sourcecode.Enclosing, inline src: sourcecode.Name)
  : IO[A] =
    logger.infoIOWithResult(body)

  def infoIOWithResult[A](function: String)(body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.infoIOWithResult(withLoc(function))(body)

  def infoIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.infoIOWithResult(withLoc(function), args, result, marker, body)

  inline def debugCall[A](inline body: => A)
    (using inline enc: sourcecode.Enclosing, inline src: sourcecode.Name)
  : A =
    logger.debugCall(body)

  def debugCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Line)
  : A =
    logger.debugCall(withLoc(functionName), args)(body)

  inline def debugIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIO(body)

  def debugIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.debugIO(withLoc(functionName), args)(body)

  inline def debugF[F[_], A](body: F[A])(using Sync[F], sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.debugF(body)

  def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : F[A] =
    logger.debugF(withLoc(functionName), args)(body)

  inline def debugIOWithResult[A](io: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIOWithResult(io)

  def debugIOWithResult[A](function: String)(body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.debugIOWithResult(withLoc(function))(body)

  def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line): IO[A] =
    logger.debugIOWithResult(withLoc(function), args)(io)

  def debugIOWithResult[A](
    function: String,
    args: => Any = "",
    result: (A => Any) | Null = null)
    (io: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.debugIOWithResult(withLoc(function), args, result)(io)

  inline def traceIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.traceIO(body)

  def traceIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line): IO[A] =
    logger.traceIO(withLoc(functionName), args)(body)

  inline def traceF[F[_], A](body: F[A])(using F: Sync[F])
    (using sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.traceF(body)

  def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : F[A] =
    logger.traceF(withLoc(functionName), args)(body)

  inline def traceIOWithResult[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.traceIOWithResult(body)

  def traceIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Line)
  : IO[A] =
    logger.traceIOWithResult(withLoc(function), args, result, marker, body)

  inline def traceCall[A](body: => A)(using sourcecode.Enclosing, sourcecode.Name): A =
    logger.traceCall(body)

  def traceCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Enclosing, sourcecode.Line)
  : A =
    logger.traceCall(withLoc(functionName), args)(body)

  def traceCallWithResult[A](function: String)(body: => A)
    (using sourcecode.Enclosing, sourcecode.Line)
  : A =
    logger.traceCallWithResult(withLoc(function))(body)

  def traceCallWithResult[A](function: String, args: => Any)(body: => A)
    (using sourcecode.Enclosing, sourcecode.Line)
  : A =
    logger.traceCallWithResult(withLoc(function), args)(body)

  def traceCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    body: => A)
    (using sourcecode.Enclosing, sourcecode.Line)
  : A =
    logger.traceCallWithResult(withLoc(function), args, result, body)

  def infoResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : Resource[F, A] =
    logger.infoResource[F, A](withLoc(function), args)(resource)

  def infoResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.infoResource[F, A](resource)

  def debugResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.debugResource[F, A](resource)

  def debugResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : Resource[F, A] =
    logger.debugResource[F, A](withLoc(function), args)(resource)

  def traceResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Resource[F, A] =
    logger.debugResource[F, A](resource)

  def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Line)
  : Resource[F, A] =
    logger.traceResource[F, A](withLoc(function), args)(resource)

  def infoStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], sourcecode.Line)
  : Stream[F, A] =
    logger.infoStream[F, A](withLoc(function), args)(stream)

  def debugStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name)
  : Stream[F, A] =
    logger.debugStream[F, A](stream)

  def debugStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], sourcecode.Line)
  : Stream[F, A] =
    logger.debugStream[F, A](withLoc(function), args)(stream)

  def traceStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Name, Tag[A])
  : Stream[F, A] =
    logger.traceStream[F, A](stream)

  def traceStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Line)
  : Stream[F, A] =
    logger.traceStream[F, A](withLoc(function), args)(stream)

  private val nameRegex = """#.*$""".r  //"""#\$.*$""".r

  private def logger(using enc: sourcecode.Enclosing): ScalaLogger =
    Logger(nameRegex.replaceFirstIn(enc.value, "")) //.replace("#", "."))

  private def withLoc(msg: String)(using enc: sourcecode.Enclosing, lineNr: sourcecode.Line)
  : String =
    val e = enc.value
    val i = e.indexOf('#')
    val method =
      if i >= 0 && e.length > i && e(i + 1) != '$' then
        " " + e.substring(i + 1)
      else
        ""
    s"${isTest ?? "🟪"}:${lineNr.value}$method - $msg"
