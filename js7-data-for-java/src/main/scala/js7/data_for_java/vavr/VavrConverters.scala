package js7.data_for_java.vavr

import io.vavr.collection.List as VList
import io.vavr.control.Either as VEither
import js7.data_for_java.common.JavaUtils.Void
import scala.jdk.CollectionConverters.*

object VavrConverters:

  implicit final class RichVavrOption[L, R](private val underlying: Either[L, R]) extends AnyVal:
    def toVavr: VEither[L, R] =
      underlying match
        case Left(o) => VEither.left(o)
        case Right(o) => VEither.right(o)

    def toVoidVavr: VEither[L, Void] =
      underlying.map(_ => Void).toVavr

  implicit final class RichVavrEitherProblem[L, R](private val underlying: VEither[L, R]) extends AnyVal:
    def toScala: Either[L, R] =
      if underlying.isLeft then Left(underlying.getLeft) else Right(underlying.get)

  implicit final class RichRawVList[A](private val underlying: Iterable[A]) extends AnyVal:
    def toVavr: VList[A] =
      VList.ofAll(underlying.asJava)
