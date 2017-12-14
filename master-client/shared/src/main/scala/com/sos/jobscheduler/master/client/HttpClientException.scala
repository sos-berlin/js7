package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.master.client.HttpClientException._

/**
  * @author Joacim Zschimmer
  */
final class HttpClientException(failure: HttpFailure) extends RuntimeException {
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
