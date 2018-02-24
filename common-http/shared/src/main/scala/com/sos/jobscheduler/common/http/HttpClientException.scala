package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.HttpClientException._

/**
  * @author Joacim Zschimmer
  */
final class HttpClientException(failure: HttpFailure) extends RuntimeException {
  override def getMessage = failure.toString
}

object HttpClientException {
  sealed trait HttpFailure

  final case class HostUnreachable(reason: String = "") extends HttpFailure {
    override def toString = s"JobScheduler Master was not reachable — $reason".trim stripSuffix " —"
  }

  final case class OtherFailure(reason: String, maybeThrowable: Option[Throwable]) extends HttpFailure {
    override def toString = reason
  }
  object OtherFailure {
    def apply(throwable: Throwable) = new OtherFailure(throwable.toStringWithCauses, Some(throwable))
  }
}
