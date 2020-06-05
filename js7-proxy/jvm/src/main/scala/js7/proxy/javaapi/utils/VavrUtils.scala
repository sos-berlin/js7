package js7.proxy.javaapi.utils

import js7.base.annotation.javaApi
import js7.base.problem.Problem
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
