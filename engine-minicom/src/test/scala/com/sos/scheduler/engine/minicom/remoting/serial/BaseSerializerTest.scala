package com.sos.scheduler.engine.minicom.remoting.serial

import com.google.common.base.Charsets.US_ASCII
import com.sos.scheduler.engine.minicom.remoting.serial.BaseSerializerTest._
import java.nio.ByteBuffer
import java.util.UUID
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.util.Random

/**
 * Tests BaseSerializer and BaseDeserializer.
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class BaseSerializerTest extends FreeSpec {

  "writeByte" in {
    new Tester[Byte](_.writeByte, _.readByte()) {
      test(1.toByte, 0x01)
      test(Byte.MaxValue, 0x7f)
      test(Byte.MinValue, 0x80)
    }
  }

  "writeInt32" in {
    new Tester[Int](_.writeInt32, _.readInt32()) {
      test(0x12345678, 0x12, 0x34, 0x56, 0x78)
      test(0x87654321, 0x87, 0x65, 0x43, 0x21) // negative
      test(Int.MaxValue, 0x7f, 0xff, 0xff, 0xff)
      test(Int.MinValue, 0x80, 0x00, 0x00, 0x00)
    }
  }

  "writeInt64" in {
    new Tester[Long](_.writeInt64, _.readInt64()) {
      test(0x1122334455667788L, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88)
      test(0x8877665544332211L, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11) // negative
      test(Long.MaxValue, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)
      test(Long.MinValue, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
    }
  }

  "writeDouble" in {
    new Tester[Double](_.writeDouble, _.readDouble()) {
      test(12345678.9012, 's', 15, '1', '.', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', 'E', '7')
      test(Double.MaxValue, 's', 22,      '1', '.', '7', '9', '7', '6', '9', '3', '1', '3', '4', '8', '6', '2', '3', '1', '5', '7', 'E', '3', '0', '8')
      test(Double.MinValue, 's', 23, '-', '1', '.', '7', '9', '7', '6', '9', '3', '1', '3', '4', '8', '6', '2', '3', '1', '5', '7', 'E', '3', '0', '8')
    }
  }

  "writeBoolean" in {
    new Tester[Boolean](_.writeBoolean, _.readBoolean()) {
      test(false, 0x00)
      test(true, 0x01)
    }
  }

  "writeString" in {
    new Tester[String](_.writeString, _.readString()) {
      test("abc»ßåÿñ«", 0x00, 0x00, 0x00, 0x09, 'a', 'b', 'c', 0xBB, 0xDF, 0xE5, 0xFF, 0xF1, 0xAB)
      test("", 0x00, 0x00, 0x00, 0x00)
    }
  }

  "writeUUID" in {
    new Tester[UUID](_.writeUUID, _.readUUID()) {
      test(UUID.fromString("11223344-5566-7788-99aa-bbccddeeff01"),
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x01)
    }
  }

  "Sequence" in {
    val bytes = Array(0x01, 0x00, 0x00, 0x00, 0x03, 'a', 'b', 'c', 0x11, 0x22, 0x33, 0x44) map { _.toByte }
    val serializer = new BaseSerializer
    serializer.writeBoolean(true)
    serializer.writeString("abc")
    serializer.writeInt32(0x11223344)
    val a: Array[Byte] = serializer.byteArrayAndLength._1 take serializer.byteArrayAndLength._2
    a shouldEqual bytes
    val deserializer = new BaseDeserializer {
      protected val buffer = ByteBuffer.wrap(bytes)
    }
    deserializer.readBoolean() shouldEqual true
    deserializer.readString() shouldEqual "abc"
    deserializer.readInt32() shouldEqual 0x11223344
  }

  "Big string" in {
    val bigString = Vector.fill(1000 * 1000) { Random.nextPrintableChar() } .mkString
    assert(bigString.length == 1000*1000)
    val lengthBytes = Vector(0, bigString.length >> 16, (bigString.length >> 8) & 0xff, bigString.length & 0xff) map { _.toByte }
    new Tester[String](_.writeString, _.readString()) {
      testSeq(bigString, lengthBytes ++ bigString.getBytes(US_ASCII))
    }
  }

  "increased" in {
    for (neededSize ← Iterator.fill(1000000) { BaseSerializer.InitialSize + Random.nextInt % 10000000 })
      BaseSerializer.increased(currentSize = BaseSerializer.InitialSize, neededSize = neededSize) should be >= neededSize
  }

  "Increased buffer" in {
    val bigString = Vector.fill(1000 * 1000) { Random.nextPrintableChar() } .mkString
    assert(bigString.length > BaseSerializer.InitialSize)
    val serializer = new BaseSerializer
    val int = 1234567
    serializer.writeInt32(int)
    // Buffer is resized now
    serializer.writeString(bigString)
    val (array, length) = serializer.byteArrayAndLength
    val deserializer = new BaseDeserializer {
      protected val buffer = ByteBuffer.wrap(array, 0, length)
    }
    deserializer.readInt32() shouldEqual int
    deserializer.readString() shouldEqual bigString
  }
}

private object BaseSerializerTest {
  private class Tester[V](serialize: BaseSerializer ⇒ (V ⇒ Unit), deserialize: BaseDeserializer ⇒ V) {
    final def test(value: V, bytes: Int*): Unit =
      testSeq(value, bytes map { _.toByte})

    final def testSeq(value: V, bytes: Seq[Byte]): Unit = {
      testSerializeSeq(value, bytes)
      testDeserializeSeq(value, bytes)
    }

    private def testSerializeSeq(value: V, bytes: Seq[Byte]): Unit = {
      val serializer = new BaseSerializer
      serialize(serializer)(value)
      val (a, length) = serializer.byteArrayAndLength
      a.take(length) shouldEqual bytes
    }

    private def testDeserializeSeq(value: V, bytes: Seq[Byte]): Unit = {
      val deserializer = new BaseDeserializer {
        protected val buffer = ByteBuffer.wrap(bytes.toArray)
      }
      deserialize(deserializer) shouldEqual value
      assert(!deserializer.hasData)
    }
  }
}
