package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCode

/**
  * @author Joacim Zschimmer
  */
final class HttpStatusCodeException(val statusCode: StatusCode, val message: String)
extends RuntimeException(s"${statusCode.intValue} ${statusCode.value} $message".trim)
