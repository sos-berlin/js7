package com.sos.jobscheduler.common.sprayutils

import spray.http.StatusCode

/**
  * @author Joacim Zschimmer
  */
final class HttpStatusCodeException(val statusCode: StatusCode, val message: String)
extends RuntimeException(s"${statusCode.intValue} ${statusCode.value} $message".trim)
