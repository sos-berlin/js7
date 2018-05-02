package com.sos.jobscheduler.common.http

import io.circe.{Decoder, Encoder}
import monix.eval.Task
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
trait HttpClient
{
  def get[A: Decoder](uri: String, timeout: Duration = Duration.Inf): Task[A]

  def post[A: Encoder, B: Decoder](uri: String, data: A): Task[B]

  def postIgnoreResponse[A: Encoder](uri: String, data: A): Task[/*StatusCode*/Int]
}
