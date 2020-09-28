package js7.base.utils

import java.io.ByteArrayInputStream
import js7.base.data.ByteArray
import js7.base.utils.InputStreams._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class InputStreamsTest extends AnyFreeSpec
{
  "inputStreamToByteArray" in {
    val bytes = new Array[Byte](10000)
    Random.nextBytes(bytes)
    assert(inputStreamToByteArray(new ByteArrayInputStream(bytes)) == ByteArray(bytes))
  }

  "inputStreamToByteArrayLimited" in {
    val bytes = new Array[Byte](101)
    Random.nextBytes(bytes)
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 99) == Left(ByteArray.fromArray(bytes, 0, 99)))
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 100) == Left(ByteArray.fromArray(bytes, 0, 100)))
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 101) == Right(ByteArray(bytes)))
    assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), 102) == Right(ByteArray(bytes)))
  }

  "inputStreamToByteArrayLimited at bufferSize" in {
    for (diff <- -2 to 2) {
      val size = 2 * InputStreams.bufferSize + diff
      val bytes = new Array[Byte](size)
      Random.nextBytes(bytes)
      assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), size - 2) == Left(ByteArray.fromArray(bytes, 0, size - 2)))
      assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), size - 1) == Left(ByteArray.fromArray(bytes, 0, size - 1)))
      assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), size + 1) == Right(ByteArray(bytes)))
      assert(inputStreamToByteArrayLimited(new ByteArrayInputStream(bytes), size + 2) == Right(ByteArray(bytes)))
    }
  }
}
