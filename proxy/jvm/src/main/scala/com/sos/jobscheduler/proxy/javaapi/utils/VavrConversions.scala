package com.sos.jobscheduler.proxy.javaapi.utils

import com.sos.jobscheduler.base.annotation.javaApi
import io.vavr.control.{Either => VEither}

@javaApi
object VavrConversions
{
  implicit final class VavrOption[L, R](private val underlying: Either[L, R]) extends AnyVal
  {
    def asVavr: VEither[L, R] =
      underlying match {
        case Left(o) => VEither.left(o)
        case Right(o) => VEither.right(o)
      }
  }
}
