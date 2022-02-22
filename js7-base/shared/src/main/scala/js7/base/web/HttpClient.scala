package js7.base.web

import io.circe.{Decoder, Encoder, Json}
import js7.base.auth.SessionToken
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.base.utils.StackTraces.StackTraceThrowable
import monix.eval.Task
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
trait HttpClient
{
  def getDecodedLinesObservable[A: Decoder](uri: Uri, responsive: Boolean = false)
    (implicit s: Task[Option[SessionToken]])
  : Task[Observable[A]]

  def getRawLinesObservable(uri: Uri)(implicit s: Task[Option[SessionToken]])
  : Task[Observable[ByteArray]]

  def get[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]]): Task[A]

  def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: Task[Option[SessionToken]])
  : Task[B]

  def postObservable[A: Encoder, B: Decoder](uri: Uri, data: Observable[A])
    (implicit s: Task[Option[SessionToken]])
  : Task[B]

  @TestOnly
  def postObservableJsonString(uri: Uri, data: Observable[String])
    (implicit s: Task[Option[SessionToken]])
  : Task[Json]

  /** Returns the HTTP status code, discarding the response data. */
  def postDiscardResponse[A: Encoder](uri: Uri, data: A, allowedStatusCodes: Set[Int] = Set.empty)
    (implicit s: Task[Option[SessionToken]])
  : Task[/*StatusCode*/Int]

  def liftProblem[A](task: Task[A]): Task[Checked[A]] =
    HttpClient.liftProblem(task)
}

object HttpClient
{
  def sessionMayBeLost(t: Throwable): Boolean =
    t match {
      case t: HttpException if t.statusInt == 401/*Unauthorized*/ || t.statusInt == 403/*Forbidden*/ => true
      case _ => false
    }

  /** Lifts a Failure(HttpException#problem) to Success(Left(problem)). */
  def liftProblem[A](task: Task[A]): Task[Checked[A]] =
    task.materialize
      .map(failureToChecked)
      .dematerialize

  def failureToChecked[A](tried: Try[A]): Try[Checked[A]] =
    tried match {
      case Failure(HttpException.HasProblem(problem)) =>
        Success(Left(problem))
      case Failure(t: HttpException) if t.getMessage != null =>
        val msg = t.getMessage + (if (t.getCause == null) "" else ", caused by " + t.getCause)
        Success(Left(Problem.withHttpStatus(msg, t, httpStatusCode = t.statusInt)))
      case Failure(t) =>
        Failure(t.appendCurrentStackTrace)
      case Success(a) =>
        Success(Right(a))
    }

  abstract class HttpException(message: String = null) extends RuntimeException(message) {
    def statusInt: Int
    def problem: Option[Problem]
    def isTemporaryUnreachable = isTemporaryUnreachableStatus(statusInt)
  }
  object HttpException {
    object HasProblem {
      def unapply(e: HttpException): Option[Problem] =
        e.problem
    }
  }

  def isTemporaryUnreachable(throwable: Throwable) =
    throwable match {
      case e: HttpClient.HttpException => e.isTemporaryUnreachable
      case _ => true  // Maybe a TCP exception
    }

  val isTemporaryUnreachableStatus = Set[Int](
    408, // Request Timeout
    429, // Too Many Requests
    //? 449, // Retry With
    502, // Bad Gateway
    503, // Service Unavailable
    504  // Gateway Timeout
  )
}
