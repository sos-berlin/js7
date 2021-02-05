package js7.data_for_java.vavr

import io.vavr.collection.{List => VavrList}
import io.vavr.control.{Either => VEither}
import js7.data_for_java.common.JavaUtils.Void
import scala.jdk.CollectionConverters._

object VavrConverters
{
  implicit final class RichVavrOption[L, R](private val underlying: Either[L, R]) extends AnyVal
  {
    def toVavr: VEither[L, R] =
      underlying match {
        case Left(o) => VEither.left(o)
        case Right(o) => VEither.right(o)
      }

    def toVoidVavr: VEither[L, Void] =
      underlying.map(_ => Void).toVavr
  }

  implicit final class RichRawVavrList[A](private val underlying: Iterable[A]) extends AnyVal
  {
    def toVavr: VavrList[A] =
      VavrList.ofAll(underlying.asJava)
  }
}
