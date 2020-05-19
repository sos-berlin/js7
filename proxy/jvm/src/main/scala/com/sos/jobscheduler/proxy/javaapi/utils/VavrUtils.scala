package com.sos.jobscheduler.proxy.javaapi.utils

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import io.vavr.control.{Either => VEither}

@javaApi
object VavrUtils
{
  @throws[RuntimeException]("iff Left")
  def getOrThrowProblem[A](either: VEither[Problem, A]): A =
    either match {
      case o: VEither.Left[Problem, A] => throw o.getLeft.throwable;
      case o: VEither.Right[Problem, A] => o.get();
    }
}
