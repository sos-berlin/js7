package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCode
import com.sos.jobscheduler.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final class HttpStatusCodeException(val statusCode: StatusCode, val problem: Problem)
extends RuntimeException
{
  override def getMessage = s"${statusCode.intValue} ${statusCode.value} $problem".trim
}
