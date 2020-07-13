package js7.proxy.javaapi.utils

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem

@javaApi
object VavrUtils
{
  @throws[RuntimeException]("iff Left")
  def getOrThrowProblem[A](either: VEither[Problem, A]): A =
    either match {
      case o: VEither.Left[Problem, A] =>
        val throwable = o.getLeft.throwable
        // Wrapping in own exception to add own stacktrace
        throw new RuntimeException(s"Operation return Left(Problem): $throwable", throwable)

      case o: VEither.Right[Problem, A] =>
        o.get();
    }
}
