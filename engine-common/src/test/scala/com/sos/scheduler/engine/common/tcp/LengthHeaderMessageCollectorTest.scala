package com.sos.scheduler.engine.common.tcp

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.LengthHeaderMessageCollector.intToBytesString
import com.sos.scheduler.engine.common.tcp.LengthHeaderMessageCollectorTest._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LengthHeaderMessageCollectorTest extends FreeSpec {

  "intToByteString" in {
    assert(intToBytesString(0) == ByteString(0, 0, 0, 0))
    assert(intToBytesString(1) == ByteString(0, 0, 0, 1))
    assert(intToBytesString(0x201) == ByteString(0, 0, 2, 1))
    assert(intToBytesString(0x30201) == ByteString(0, 3, 2, 1))
    assert(intToBytesString(0x4030201) == ByteString(4, 3, 2, 1))
    assert(intToBytesString(0x8a8b8c8d) == ByteString(0x8a, 0x8b, 0x8c, 0x8d))
  }

  private val collector = new LengthHeaderMessageCollector

  "Zero length" in {
    assert(collector.isReset)
    assert(collector(ByteString(0, 0, 0, 0)) contains ByteString())
    assert(collector.isReset)
  }

  "Nothing" in {
    assert(collector(ByteString()).isEmpty)
    assert(collector.isReset)
  }

  "One chunk" in {
    assert(collector(ByteString(0, 0, 0, 2, 11, 22)) contains ByteString(11, 22))
    assert(collector.isReset)
  }

  "Two chunks" in {
    assert(collector(ByteString(0, 0, 0, 3)).isEmpty)
    assert(!collector.isReset)
    assert(collector(ByteString(33, 44, 55)) contains ByteString(33, 44, 55))
    assert(collector.isReset)
  }

  "Chunk with incomplete length" in {
    assert(collector(ByteString(0, 0, 0)).isEmpty)
    assert(!collector.isReset)
    assert(collector(ByteString(4, 66, 77, 88, 99)) contains ByteString(66, 77, 88, 99))
    assert(collector.isReset)
  }

  "All four length bytes used, 24MB, random chunk sizes" in {
    val length = (1 << 24) | (200 << 16) | (33 << 8) | 4
    val expectedContent = new Array[Byte](length)
    val chunkSize = 100000
    for (_ ← 1 to 3) {
      assert(collector(ByteString(1)).isEmpty)
      assert(collector(ByteString(200, 33, 4)).isEmpty)
      Random.nextBytes(expectedContent)
      for (i ← 0 until length / chunkSize) {
        val chunk = ByteString.fromArray(expectedContent, i * chunkSize, length = chunkSize)
        val complete: Option[ByteString] = collector(chunk)
        assert(complete.isEmpty)
      }
      val remaining = length % chunkSize
      val lastChunk = ByteString.fromArray(expectedContent, length - remaining, length = remaining)
      val result = collector(lastChunk)
      if (!(result.get.toArray[Byte] sameElements expectedContent)) fail(s"$collector has invalid content")
      // Slow: if (!(result contains expectedContent)) fail(s"$collector has invalid content")  // assert macro would blow up heap
      assert(collector.isReset)
    }
  }

  "Speed test" in {
    // Similar to the test before, but without slow tests
    val length = 10*1000*1000 + 1
    val chunkSize = 10000
    val chunkArray = new Array[Byte](chunkSize)
    Random.nextBytes(chunkArray)
    val chunk = ByteString(chunkArray)
    val lastChunk = ByteString.fromArray(chunkArray, 0, length = length % chunkSize)
    val start = now()
    val n = 1000
    for (_ ← 1 to n) {
      assert(collector.isReset)
      collector(intToBytesString(length))
      for (i ← 0 until length / chunkSize) collector(chunk)
      val result = collector(lastChunk)
      if (result.get.size != length) fail()
    }
    val totalDuration = now - start
    logger.info(s"${(totalDuration / n).pretty} (${totalDuration.pretty}/$n)")
    assert(totalDuration < 2.s)
  }
}

private object LengthHeaderMessageCollectorTest {
  private val logger = Logger(getClass)
}
