package js7.proxy.javaapi.data.common

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.proxy.javaapi.data.common.VavrConverters._

@javaApi
object VavrUtils
{
  @javaApi
  @throws[RuntimeException]("iff Left")
  def getOrThrow[A](either: VEither[Problem, A]): A = {
    either match {
      case o: VEither.Left[Problem, A] =>
        val throwable = o.getLeft.throwable
        // Wrapping in own exception to add own stacktrace
        throw new RuntimeException(s"Operation returned Left(Problem): $throwable", throwable)

      case o: VEither.Right[Problem, A] =>
        o.get();
    }
  }

  @javaApi
  @throws[RuntimeException]("iff Left")
  def getOrThrow[A](either: Either[Problem, A]): A =
    getOrThrow(either.toVavr)
}
