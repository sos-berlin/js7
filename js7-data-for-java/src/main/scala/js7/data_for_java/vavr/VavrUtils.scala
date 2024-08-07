package js7.data_for_java.vavr

import io.vavr.control.Either as VEither
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit.SECONDS
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.data_for_java.vavr.VavrConverters.*

@javaApi
object VavrUtils:

  /** For testing. */
  @javaApi
  //Void is null: @Nonnull
  @throws[RuntimeException]("iff Left or timeout")
  def await[A](@Nonnull future: CompletableFuture[VEither[Problem, A]]): A =
    getOrThrow(future.get(99, SECONDS))

  @javaApi
  //Void is null: @Nonnull
  @throws[RuntimeException]("iff Left")
  def getOrThrow[A](@Nonnull either: VEither[Problem, A]): A =
    either match
      case o: VEither.Left[Problem, A] =>
        val throwable = o.getLeft.throwable
        // Wrapping in own exception to add own stacktrace
        throw new RuntimeException(throwable.toStringWithCauses, throwable)

      case o: VEither.Right[Problem, A] =>
        o.get();

  @javaApi
  //Void is null: @Nonnull
  @throws[RuntimeException]("iff Left")
  def getOrThrow[A](@Nonnull checked: Checked[A]): A =
    getOrThrow(checked.toVavr)

  @javaApi
  def toVavr[L, R](@Nonnull either: Either[L, R]): VEither[L, R] =
    either.toVavr
