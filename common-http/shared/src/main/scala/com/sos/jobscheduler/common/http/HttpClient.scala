package com.sos.jobscheduler.common.http

import io.circe.{Decoder, Encoder}
import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
trait HttpClient {
  def get[A: Decoder](uri: String, timeout: Duration = Duration.Inf): Future[A]

  def post[A: Encoder, B: Decoder](uri: String, data: A): Future[B]

  def postIgnoreResponse[A: Encoder](uri: String, data: A): Future[/*StatusCode*/Int]
}
