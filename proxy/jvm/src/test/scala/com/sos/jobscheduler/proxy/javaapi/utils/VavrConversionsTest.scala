package com.sos.jobscheduler.proxy.javaapi.utils

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.proxy.javaapi.utils.VavrConversions._
import io.vavr.control.{Either => VEither}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
@javaApi
final class VavrConversionsTest extends AnyFreeSpec
{
  "Either" in {
    assert(Left(7).asVavr == VEither.left(7))
    assert(Right(7).asVavr == VEither.right(7))
  }
}
