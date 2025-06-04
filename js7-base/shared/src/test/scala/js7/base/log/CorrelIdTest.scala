package js7.base.log

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.US_ASCII
import java.util.Base64
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.CorrelId.LongCorrelId
import js7.base.log.CorrelIdTest.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.Deadline
import scala.util.Random

final class CorrelIdTest extends OurTestSuite:

  override protected def suppressTestCorrelId = true

  "JSON" in:
    testJson(CorrelId(0xba9876543210L), json""" "tog1UCHP"  """)
    testJson(CorrelId(0), json""" "________"  """)
    testJson(CorrelId(1), json""" "_______A"  """)
    testJson(CorrelId(26), json""" "_______Z"  """)
    testJson(CorrelId(62), json""" "_______9"  """)
    testJson(CorrelId(63), json""" "_______ñ"  """)

  "apply" in:
    assert(CorrelId("A") == CorrelId("A_______"))

  "checked" in:
    assert(CorrelId.checked("") == Right(CorrelId.empty))
    assert(CorrelId.checked("?") == Left(Problem("Invalid CorrelId")))
    assert(CorrelId.checked("123456789") == Left(Problem("Invalid CorrelId")))
    assert(CorrelId.checked("A") == Right(CorrelId("A_______")))
    assert(CorrelId.checked("A_12345ñ") == Right(CorrelId("A_12345ñ")))
    assert(CorrelId.checked("A_12345-") == Right(CorrelId("A_12345ñ")))

  "isEmpty" in:
    assert(CorrelId.empty.isEmpty)
    assert(CorrelId("").isEmpty) // Should not occur


  "width" in:
    assert(CorrelId.width == 8)

  "bitMask" in:
    assert( CorrelId.bitMask ==     0xffffffffffffL)
    assert(~CorrelId.bitMask == 0xffff000000000000L)

  "toBase64, fromBase64" in:
    val base64Encoder = Base64.getUrlEncoder.withoutPadding

    def classicBaseTo64(long: Long): String =
      val array = new Array[Byte](CorrelId.longByteCount)
      val buffer = ByteBuffer.wrap(array)
      buffer.putShort(((long >>> 32) & 0xffff).toShort)
      buffer.putInt((long & 0xffffffffL).toInt)
      val encoded = base64Encoder.encode(array.take(CorrelId.width))
      new String(encoded, US_ASCII)

    for long <- Iterator.fill(10000)(Random.nextLong(1L << (CorrelId.longByteCount * 8))) do
      assert(LongCorrelId.fromBase64(LongCorrelId.toBase64(long)) == Right(long))

  "generate" in:
    if !CorrelId.isEnabled then
      //println("❗CorrelId is disabled❗️")
      pending
    else
      val correlId = CorrelId.generate()
      assert(!correlId.isEmpty)
      assert(correlId.string.length == CorrelId.width)

  "bind[Unit]" in:
    pending // Since Cats Effects
    assert(CorrelId.current.isEmpty)
    var a: CorrelId = null.asInstanceOf[CorrelId]
    CorrelId("__SYNC__").bind:
      a = CorrelId.current
    assert(CorrelId.current.isEmpty)
    assert(a == CorrelId("__SYNC__"))

  "bindNow" in:
    pending // Since Cats Effects
    assert(CorrelId.current.isEmpty)
    var a: CorrelId = null.asInstanceOf[CorrelId]
    CorrelId("__SYNC__").bindNow:
      a = CorrelId.current
      7  // A type other than Unit requires bindNow
    assert(CorrelId.current.isEmpty)
    assert(a == CorrelId("__SYNC__"))

  "bind[String]" in:
    pending // Since Cats Effects
    import CanBindCorrelId.implicits.synchronousAsDefault
    assert(CorrelId.current.isEmpty)
    val result = CorrelId("__SYNC__").bind:
      CorrelId.current.string
    assert(CorrelId.current.isEmpty)
    assert(result == "__SYNC__")

  "fold" in:
    assert(CorrelId.empty.fold("EMPTY", _ => fail()) == "EMPTY")
    assert(CorrelId("_FILLED_").fold("EMPTY", o => s"*$o*") == "*_FILLED_*")

  "speed" - {
    val n = if sys.props.contains("test.speed") then 1_000_000 else 100_000

    "CorrelId.next" in:
      for _ <- 1 to 3 do
        val since = Deadline.now
        for _ <- 1 to n do
          garbage = CorrelId.generate()
        logger.info(itemsPerSecondString(since.elapsed, n, "CorrelIds"))

    "CorrelId.string" in:
      for _ <- 1 to 3 do
        val since = Deadline.now
        for _ <- 1 to n do
          garbage = CorrelId.generate().string
        logger.info(itemsPerSecondString(since.elapsed, n, "strings"))
  }

object CorrelIdTest:
  var garbage: Any = null
  private val logger = Logger[this.type]
