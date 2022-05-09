package js7.base.log

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.US_ASCII
import java.util.Base64
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.CorrelId.LongCorrelId
import js7.base.log.CorrelIdTest._
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline
import scala.util.Random

final class CorrelIdTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(CorrelId(0xba9876543210L), json""" "uph2VDIQ"  """)
    testJson(CorrelId(CorrelId.bitMask), json""" "________"  """)
    testJson(CorrelId(62), json""" "AAAAAAAñ"  """)
    testJson(CorrelId(63), json""" "AAAAAAA_"  """)
  }

  "isEmpty" in {
    assert(CorrelId.empty.isEmpty)
    assert(CorrelId("").isEmpty) // Should not occur
    assert(!CorrelId.generate().isEmpty)
  }

  "width" in {
    assert(CorrelId.width == 8)
    assert(CorrelId.generate().string.length == CorrelId.width)
  }

  "bitMask" in {
    assert( CorrelId.bitMask ==     0xffffffffffffL)
    assert(~CorrelId.bitMask == 0xffff000000000000L)
  }

  "toBase64, fromBase64" in {
    val base64Encoder = Base64.getUrlEncoder.withoutPadding

    def classicBaseTo64(long: Long): String = {
      val array = new Array[Byte](CorrelId.longByteCount)
      val buffer = ByteBuffer.wrap(array)
      buffer.putShort(((long >>> 32) & 0xffff).toShort)
      buffer.putInt((long & 0xffffffff).toInt)
      val encoded = base64Encoder.encode(array.take(CorrelId.width))
      new String(encoded, US_ASCII)
    }

    for (long <- Iterator.fill(10000)(Random.nextLong(1L << (CorrelId.longByteCount * 8)))) {
      assert(LongCorrelId.fromBase64(LongCorrelId.toBase64(long)) == Right(long))
    }
  }

  "speed" - {
    val n = if (sys.props.contains("test.speed")) 1_000_000 else 100_000

    "CorrelId.next" in {
      for (_ <- 1 to 3) {
        val since = Deadline.now
        for (_ <- 1 to n) {
          garbage = CorrelId.generate()
        }
        scribe.info(itemsPerSecondString(since.elapsed, n, "CorrelIds"))
      }
    }

    "CorrelId.string" in {
      for (_ <- 1 to 3) {
        val since = Deadline.now
        for (_ <- 1 to n) {
          garbage = CorrelId.generate().string
        }
        scribe.info(itemsPerSecondString(since.elapsed, n, "strings"))
      }
    }
  }
}

object CorrelIdTest {
  var garbage: Any = null
}
