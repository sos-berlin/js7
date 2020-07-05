package js7.base.utils

import java.io.ByteArrayInputStream
import js7.base.utils.InputStreams._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class InputStreamsTest extends AnyFreeSpec
{
  "inputStreamToByteVector" in {
    val bytes = new Array[Byte](10000)
    Random.nextBytes(bytes)
    assert(inputStreamToByteVector(new ByteArrayInputStream(bytes)) == ByteVector(bytes))
  }

  "inputStreamToByteVectorLimited" in {
    val bytes = new Array[Byte](101)
    Random.nextBytes(bytes)
    assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), 99) == Left(ByteVector(bytes, 0, 99)))
    assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), 100) == Left(ByteVector(bytes, 0, 100)))
    assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), 101) == Right(ByteVector(bytes)))
    assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), 102) == Right(ByteVector(bytes)))
  }

  "inputStreamToByteVectorLimited at bufferSize" in {
    for (diff <- -2 to 2) {
      val size = 2 * InputStreams.bufferSize + diff
      val bytes = new Array[Byte](size)
      Random.nextBytes(bytes)
      assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), size - 2) == Left(ByteVector(bytes, 0, size - 2)))
      assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), size - 1) == Left(ByteVector(bytes, 0, size - 1)))
      assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), size + 1) == Right(ByteVector(bytes)))
      assert(inputStreamToByteVectorLimited(new ByteArrayInputStream(bytes), size + 2) == Right(ByteVector(bytes)))
    }
  }

  "inputStreamToByteArrayLimited" in {
    val bytes = new Array[Byte](101)
    Random.nextBytes(bytes)
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 99).left.get sameElements bytes.take(99))
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 100).left.get sameElements bytes.take(100))
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 101).getOrElse(fail()) sameElements bytes)
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 102).getOrElse(fail()) sameElements bytes)
  }
}
