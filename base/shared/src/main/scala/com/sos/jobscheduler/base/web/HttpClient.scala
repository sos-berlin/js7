package com.sos.jobscheduler.base.web

import io.circe.{Decoder, Encoder}
import monix.eval.Task
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
trait HttpClient
{
  def get[A: Decoder](uri: String, timeout: Duration = Duration.Inf): Task[A]

  def post[A: Encoder, B: Decoder](uri: String, data: A, suppressSessionToken: Boolean = false): Task[B]

  /** Returns the HTTP status code, discarding the response data. */
  def postDiscardResponse[A: Encoder](uri: String, data: A): Task[/*StatusCode*/Int]
}

object HttpClient {
  abstract class HttpException(message: String) extends RuntimeException(message) {
    def statusInt: Int
    def isUnreachable = isUnreachableStatus(statusInt)
  }

  private val isUnreachableStatus = Set[Int](
    408, // Request Timeout
    429, // Too Many Requests
    //? 449, // Retry With
    502, // Bad Gateway
    503, // Service Unavailable
    504  // Gateway Timeout
  )
}
