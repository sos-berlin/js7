package com.sos.jobscheduler.http.client.idempotence

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.http.client.idempotence.IdempotentHeaders.`X-JobScheduler-Request-ID`
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IdempotentHeadersTest extends FreeSpec {

  private val requestIdNumber = 123567890123456789L
  private val requestId = RequestId(requestIdNumber)

  "X-JobScheduler-Request-ID" in {
    val name = "X-JobScheduler-Request-ID"
    val value = s"$requestIdNumber PT7S"
    val headerLine = s"$name: $value"
    val header = `X-JobScheduler-Request-ID`(requestId, Some(7.s))
    assert(header.toString == headerLine)

    val `X-JobScheduler-Request-ID`.Value(requestId_, duration) = value
    assert(requestId_ == requestId && duration == Some(7.s))
  }

  "X-JobScheduler-Request-ID without lifetime" in {
    val name = "X-JobScheduler-Request-ID"
    val value = s"$requestIdNumber PT277777H46M39S"
    val headerLine = s"$name: $value"
    val header = `X-JobScheduler-Request-ID`(requestId)
    assert(header.toString == headerLine)

    val `X-JobScheduler-Request-ID`.Value(requestId_, duration) = value
    assert(requestId_ == requestId && duration == None)
  }
}
