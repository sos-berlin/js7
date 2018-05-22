package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.HttpClientException._
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class HttpClientException private[http](val reason: Reason) extends RuntimeException with NoStackTrace
{
  override def getMessage = reason.toString

  override def toString = reason.toString
}

object HttpClientException {
  sealed trait Reason {
    def isUnreachable: Boolean = false
  }

  final case class HttpFailure(status: Int, statusText: String) extends Reason {
    override def isUnreachable = status == 502  // Bad Gateway (Master is unreachable)

    override def toString = s"JobScheduler returns HTTP status $status $statusText"
  }

  final case class HostUnreachable(reason: String = "") extends Reason {
    override def isUnreachable = true

    override def toString = s"JobScheduler is unreachable — $reason".trim stripSuffix " —"
  }

  final case class OtherFailure(reason: String, maybeThrowable: Option[Throwable]) extends Reason {
    override def toString = reason
  }
  object OtherFailure {
    def apply(throwable: Throwable) = new OtherFailure(throwable.toStringWithCauses, Some(throwable))
  }
}
