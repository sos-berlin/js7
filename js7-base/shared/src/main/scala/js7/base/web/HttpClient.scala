package js7.base.web

import io.circe.{Decoder, Encoder, Json}
import js7.base.auth.SessionToken
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.base.utils.StackTraces.StackTraceThrowable
import cats.effect.IO
import fs2.Stream
import js7.base.monixlike.MonixLikeExtensions.{dematerialize, materialize}
import js7.base.utils.UseDefault
import org.jetbrains.annotations.TestOnly
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
trait HttpClient:

  def getDecodedLinesStream[A: Decoder](
    uri: Uri,
    responsive: Boolean = false,
    returnHeartbeatAs: Option[ByteArray] = None,
    prefetch: Int | UseDefault = UseDefault)
    (using s: IO[Option[SessionToken]])
  : IO[Stream[IO, A]]

  def getRawLinesStream(
    uri: Uri,
    returnHeartbeatAs: Option[ByteArray] = None)
    (using s: IO[Option[SessionToken]])
  : IO[Stream[IO, ByteArray]]

  def get[A: Decoder](uri: Uri)(implicit s: IO[Option[SessionToken]]): IO[A]

  def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: IO[Option[SessionToken]])
  : IO[B]

  def postStream[A: Encoder, B: Decoder](
    uri: Uri,
    data: Stream[IO, A],
    responsive: Boolean = false,
    terminateStreamOnCancel: Boolean = false)
    (implicit s: IO[Option[SessionToken]])
  : IO[B]

  @TestOnly
  def postJsonStringStream(uri: Uri, data: Stream[IO, String])
    (implicit s: IO[Option[SessionToken]])
  : IO[Json]

  /** Returns the HTTP status code, discarding the response data. */
  def postDiscardResponse[A: Encoder](uri: Uri, data: A, allowedStatusCodes: Set[Int] = Set.empty)
    (implicit s: IO[Option[SessionToken]])
  : IO[/*StatusCode*/Int]

  def liftProblem[A](io: IO[A]): IO[Checked[A]] =
    HttpClient.liftProblem(io)


object HttpClient:
  def sessionMayBeLost(t: Throwable): Boolean =
    t match
      case t: HttpException if t.statusInt == 401/*Unauthorized*/ || t.statusInt == 403/*Forbidden*/ => true
      case _ => false

  /** Lifts a Failure(HttpException#problem) to Success(Left(problem)). */
  def liftProblem[A](io: IO[A]): IO[Checked[A]] =
    io.materialize
      .map(failureToChecked)
      .dematerialize

  def failureToChecked[A](tried: Try[A]): Try[Checked[A]] =
    tried match
      case Failure(throwable) => throwableToTry(throwable).map(Left(_))
      case Success(a) => Success(Right(a))

  def attemptedToChecked[A](either: Either[Throwable, A]): Either[Throwable, Checked[A]] =
    either match
      case Left(throwable) => throwableToTry(throwable).map(Left(_)).toEither
      case Right(a) => Right(Right(a))

  def throwableToProblem(throwable: Throwable): Problem =
    throwableToTry(throwable) match
      case Failure(throwable) => Problem.fromThrowable(throwable)
      case Success(problem) => problem

  private def throwableToTry(throwable: Throwable): Try[Problem] =
    throwable match
      case HttpException.HasProblem(problem) =>
        Success(problem)
      case t: HttpException if t.getMessage != null =>
        val msg = t.getMessage + (if t.getCause == null then "" else ", caused by " + t.getCause)
        Success(Problem.withHttpStatus(msg, t, httpStatusCode = t.statusInt))
      case t =>
        Failure(t.appendCurrentStackTrace)

  abstract class HttpException(message: String = null) extends RuntimeException(message):
    def statusInt: Int
    def problem: Option[Problem]
    def isTemporaryUnreachable: Boolean =
      isTemporaryUnreachableStatus(statusInt)
  object HttpException:
    object HasProblem:
      def unapply(e: HttpException): Option[Problem] =
        e.problem

  def isTemporaryUnreachable(throwable: Throwable): Boolean =
    throwable match
      case e: HttpClient.HttpException => e.isTemporaryUnreachable
      case _ => true  // Maybe a TCP exception

  val isTemporaryUnreachableStatus: Set[Int] = Set[Int](
    408, // Request Timeout
    429, // Too Many Requests
    //? 449, // Retry With
    502, // Bad Gateway
    503, // Service Unavailable
    504  // Gateway Timeout
  )
