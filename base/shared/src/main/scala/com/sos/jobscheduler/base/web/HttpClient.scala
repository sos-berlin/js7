package com.sos.jobscheduler.base.web

import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
trait HttpClient
{
  def getDecodedLinesObservable[A: Decoder](uri: Uri)(implicit s: Option[SessionToken]): Task[Observable[A]]

  def getRawLinesObservable(uri: Uri)(implicit s: Option[SessionToken]): Task[Observable[ByteVector]]

  def get[A: Decoder](uri: Uri, timeout: Duration = Duration.Inf)(implicit s: Option[SessionToken]): Task[A]

  def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: Option[SessionToken]): Task[B]

  /** Returns the HTTP status code, discarding the response data. */
  def postDiscardResponse[A: Encoder](uri: Uri, data: A, allowedStatusCodes: Set[Int] = Set.empty)
    (implicit s: Option[SessionToken])
  : Task[/*StatusCode*/Int]

  def liftProblem[A](task: Task[A]): Task[Checked[A]] =
    HttpClient.liftProblem(task)
}

object HttpClient
{
  /** Lifts a Failure(HttpException#problem) to Success(Left(problem)). */
  def liftProblem[A](task: Task[A]): Task[Checked[A]] =
    task.materialize.map {
      case Failure(t: HttpException) =>
        t.problem match {
          case None => Failure(t.appendCurrentStackTrace)
          case Some(problem) => Success(Left(problem))
        }
      case Failure(t) =>
        Failure(t.appendCurrentStackTrace)
      case Success(a) =>
        Success(Right(a))
    }
    .dematerialize

  abstract class HttpException(message: String) extends RuntimeException(message) {
    def statusInt: Int
    def problem: Option[Problem]
    def isTemporaryUnreachable = isTemporaryUnreachableStatus(statusInt)
  }

  private val isTemporaryUnreachableStatus = Set[Int](
    408, // Request Timeout
    429, // Too Many Requests
    //? 449, // Retry With
    502, // Bad Gateway
    503, // Service Unavailable
    504  // Gateway Timeout
  )
}
