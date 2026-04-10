package js7.base.log

import cats.effect.{IO, Resource, Sync}
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.log.Logger.syntax.*
import js7.base.scalasource.ScalaSourceLocation
import org.jetbrains.annotations.TestOnly
import org.slf4j.Marker

/** AdHocLogger is used without a logger variable.
 * Not efficient but easy to use. Meant for testing. */
transparent trait AdHocLogger:
  this: Logger.type =>

  @TestOnly
  def error(msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.error(withLoc(msg))

  @TestOnly
  def error(msg: String, throwable: Throwable)(using sourcecode.Enclosing, ScalaSourceLocation)
  : Unit =
    logger.error(withLoc(msg), throwable)

  @TestOnly
  def warn(msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.warn(withLoc(msg))

  @TestOnly
  def warn(msg: String, throwable: Throwable)(using sourcecode.Enclosing, ScalaSourceLocation)
  : Unit =
    logger.warn(withLoc(msg), throwable)

  @TestOnly
  def info(msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.info(withLoc(msg))

  @TestOnly
  def debug(msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.debug(withLoc(msg))

  @TestOnly
  def trace(msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.trace(withLoc(msg))

  inline def isEnabled(level: LogLevel): Boolean =
    logger.isEnabled(level)

  inline def isEnabled(level: LogLevel, marker: Marker): Boolean =
    logger.isEnabled(level, marker)

  @TestOnly
  def log(level: LogLevel, msg: String)(using sourcecode.Enclosing, ScalaSourceLocation): Unit =
    logger.log(level, withLoc(msg))

  @TestOnly
  def log(level: LogLevel, msg: String, throwable: Throwable)
    (using enc: sourcecode.Enclosing, loc: ScalaSourceLocation)
  : Unit =
    logger.log(level, withLoc(msg), throwable)

  @TestOnly
  def log(level: LogLevel, marker: Marker, msg: String)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : Unit =
    logger.log(level, marker, withLoc(msg))

  @TestOnly
  inline def infoCall[A](inline body: => A)(using inline name: sourcecode.Name): A =
    infoCall[A](defaultFunctionName)(body)

  @TestOnly
  def infoCall[A](functionName: String, args: => Any = "")(body: => A)
    (using ScalaSourceLocation)
  : A =
    logger.infoCall(withLoc(functionName), args)(body)

  inline def infoCallWithResult[A](inline body: => A)(using inline src: sourcecode.Name): A =
    logger.infoCallWithResult(body)

  @TestOnly
  def infoCallWithResult[A](function: String)(body: => A)(using ScalaSourceLocation): A =
    logger.infoCallWithResult(withLoc(function))(body)

  @TestOnly
  def infoCallWithResult[A](function: String, args: => Any)(body: => A)(using ScalaSourceLocation)
  : A =
    logger.infoCallWithResult(withLoc(function), args)(body)

  @TestOnly
  def infoCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: => A)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : A =
    logger.infoCallWithResult(withLoc(function), args, result, marker, body)

  @TestOnly
  def infoIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
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

  @TestOnly
  def infoF[F[_], A](functionName: String, args: => Any = "")
    (body: => F[A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : F[A] =
    logger.infoF(withLoc(functionName), args)(body)

  inline def infoIOWithResult[A](inline body: IO[A])
    (using inline enc: sourcecode.Enclosing, inline src: sourcecode.Name)
  : IO[A] =
    logger.infoIOWithResult(body)

  @TestOnly
  def infoIOWithResult[A](function: String)(body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.infoIOWithResult(withLoc(function))(body)

  @TestOnly
  def infoIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.infoIOWithResult(withLoc(function), args, result, marker, body)

  inline def debugCall[A](inline body: => A)
    (using inline enc: sourcecode.Enclosing, inline src: sourcecode.Name)
  : A =
    logger.debugCall(body)

  @TestOnly
  def debugCall[A](functionName: String, args: => Any = "")(body: => A)
    (using ScalaSourceLocation)
  : A =
    logger.debugCall(withLoc(functionName), args)(body)

  inline def debugIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIO(body)

  @TestOnly
  def debugIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.debugIO(withLoc(functionName), args)(body)

  inline def debugF[F[_], A](body: F[A])(using Sync[F], sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.debugF(body)

  @TestOnly
  def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : F[A] =
    logger.debugF(withLoc(functionName), args)(body)

  inline def debugIOWithResult[A](io: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.debugIOWithResult(io)

  @TestOnly
  def debugIOWithResult[A](function: String)(body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.debugIOWithResult(withLoc(function))(body)

  @TestOnly
  def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation): IO[A] =
    logger.debugIOWithResult(withLoc(function), args)(io)

  @TestOnly
  def debugIOWithResult[A](
    function: String,
    args: => Any = "",
    result: (A => Any) | Null = null)
    (io: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.debugIOWithResult(withLoc(function), args, result)(io)

  inline def traceIO[A](body: IO[A])(using sourcecode.Enclosing, sourcecode.Name): IO[A] =
    logger.traceIO(body)

  @TestOnly
  def traceIO[A](functionName: String, args: => Any = "")(body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation): IO[A] =
    logger.traceIO(withLoc(functionName), args)(body)

  inline def traceF[F[_], A](body: F[A])(using F: Sync[F])
    (using sourcecode.Enclosing, sourcecode.Name)
  : F[A] =
    logger.traceF(body)

  @TestOnly
  def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : F[A] =
    logger.traceF(withLoc(functionName), args)(body)

  inline def traceIOWithResult[A](body: IO[A])
    (using sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : IO[A] =
    traceIOWithResult(defaultFunctionName, body = body)

  @TestOnly
  def traceIOWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    marker: Marker | Null = null,
    body: IO[A])
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : IO[A] =
    logger.traceIOWithResult(withLoc(function), args, result, marker, body)

  inline def traceCall[A](body: => A)
    (using sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : A =
    traceCall(defaultFunctionName)(body)

  @TestOnly
  def traceCall[A](functionName: String, args: => Any = "")(body: => A)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : A =
    logger.traceCall(withLoc(functionName), args)(body)

  @TestOnly
  def traceCallWithResult[A](function: String)(body: => A)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : A =
    logger.traceCallWithResult(withLoc(function))(body)

  @TestOnly
  def traceCallWithResult[A](function: String, args: => Any)(body: => A)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : A =
    logger.traceCallWithResult(withLoc(function), args)(body)

  @TestOnly
  def traceCallWithResult[A](
    function: String,
    args: => Any = "",
    result: A => Any = identity[A],
    body: => A)
    (using sourcecode.Enclosing, ScalaSourceLocation)
  : A =
    logger.traceCallWithResult(withLoc(function), args, result, body)

  @TestOnly
  def infoResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : Resource[F, A] =
    infoResource[F, A](defaultFunctionName)(resource)

  @TestOnly
  def infoResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : Resource[F, A] =
    logger.infoResource[F, A](withLoc(function), args)(resource)

  @TestOnly
  def debugResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : Resource[F, A] =
    debugResource[F, A](defaultFunctionName)(resource)

  @TestOnly
  def debugResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : Resource[F, A] =
    logger.debugResource[F, A](withLoc(function), args)(resource)

  @TestOnly
  def traceResource[F[_], A](resource: Resource[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : Resource[F, A] =
    debugResource[F, A](defaultFunctionName)(resource)

  @TestOnly
  def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
    (using Sync[F], ScalaSourceLocation)
  : Resource[F, A] =
    logger.traceResource[F, A](withLoc(function), args)(resource)

  @TestOnly
  def infoStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], ScalaSourceLocation)
  : Stream[F, A] =
    logger.infoStream[F, A](withLoc(function), args)(stream)

  @TestOnly
  def debugStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], Tag[A], sourcecode.Enclosing, sourcecode.Name, ScalaSourceLocation)
  : Stream[F, A] =
    debugStream[F, A](defaultFunctionName)(stream)

  @TestOnly
  def debugStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], ScalaSourceLocation)
  : Stream[F, A] =
    logger.debugStream[F, A](withLoc(function), args)(stream)

  @TestOnly
  def traceStream[F[_], A](stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing, sourcecode.Name, Tag[A], ScalaSourceLocation)
  : Stream[F, A] =
    traceStream[F, A](defaultFunctionName)(stream)

  @TestOnly
  def traceStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
    (using Sync[F], sourcecode.Enclosing, ScalaSourceLocation)
  : Stream[F, A] =
    logger.traceStream[F, A](withLoc(function), args)(stream)

  private val nameRegex = """#.*$""".r  //"""#\$.*$""".r

  private def logger(using enc: sourcecode.Enclosing): ScalaLogger =
    Logger(nameRegex.replaceFirstIn(enc.value, "")) //.replace("#", "."))

  private def defaultFunctionName(using name: sourcecode.Name, loc: ScalaSourceLocation): String =
    withLoc(name.value)

  private def withLoc(msg: String)(using loc: ScalaSourceLocation): String =
    s"$loc 🟪 $msg"
