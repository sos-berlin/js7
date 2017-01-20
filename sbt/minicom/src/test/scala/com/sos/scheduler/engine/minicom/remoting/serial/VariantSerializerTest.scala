package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.remoting.serial.VariantSerializerTest._
import com.sos.scheduler.engine.minicom.remoting.serial.variantTypes._
import com.sos.scheduler.engine.minicom.types.VariantArray
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class VariantSerializerTest extends FreeSpec {

  "Byte - not implemented" in {
    pendingUntilFixed {
      test(77.toByte, VT_I1, List(77))
    }
  }

  "Int" in {
    test           (0x11223344, VT_I4 , List(0x11, 0x22, 0x33, 0x44))
    testDeserialize(0x11223344, VT_INT, List(0x11, 0x22, 0x33, 0x44))
  }

  "Long" in {
    test(0x1122334455667788L, VT_I8 , List(0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88))
  }

  "Boolean" in {
    test(true, VT_BOOL, List(1))
  }

  "String" in {
    test("abc", VT_BSTR, List(0, 0, 0, 3, 'a', 'b', 'c'))
  }

  "EmptyVariant, Unit" in {
    testSerialize((), VT_EMPTY, Nil)
  }

  "Array" in {
    testSerialize(Array(1, 2, 3),
      VT_VARIANT | VT_ARRAY, ArraySerialized)
  }

  "Seq" in {
    testSerialize(Seq(1, 2, 3),
      VT_VARIANT | VT_ARRAY, ArraySerialized)
  }

  "VariantArray" in {
    test(VariantArray(Vector(1, 2, 3)),
      VT_VARIANT | VT_ARRAY, ArraySerialized)
  }
}

private object VariantSerializerTest {

  private val ArraySerialized = Vector(
    0, 1,  // Dimensions
    0x08, 0x80, // Features FADF_HAVEVARTYPE | FADF_VARIANT
    0, 0, 0, 3,  // Count
    0, 0, 0, 0,  // Start index
    0, 0, 0, VT_VARIANT,  // Element type
    0, 0, 0, VT_I4, 0, 0, 0, 1,
    0, 0, 0, VT_I4, 0, 0, 0, 2,
    0, 0, 0, VT_I4, 0, 0, 0, 3)

  private def test(value: Any, variantType: Int, bytes: Seq[Int]): Unit = {
    testSerialize(value, variantType, bytes)
    testDeserialize(value, variantType, bytes)
  }

  private def testSerialize(value: Any, variantType: Int, bytes: Seq[Int]): Unit = {
    val serializer = new VariantSerializer.WithoutIUnknown
    serializer.writeVariant(value)
    serializer.toByteString shouldEqual variantTypeToByteString(variantType, bytes)
  }

  private def testDeserialize(value: Any, variantType: Int, bytes: Seq[Int]): Unit = {
    val deserializer = new VariantDeserializer with NoIUnknownDeserializer {
      protected val buffer = variantTypeToByteString(variantType, bytes).asByteBuffer
    }
    deserializer.readVariant() shouldEqual value
    assert(!deserializer.hasData)
  }

  private def variantTypeToByteString(variantType: Int, bytes: Seq[Int]) =
    ByteString.fromArray(intToBytes(variantType) ++ (bytes map { _.toByte }))

  private def intToBytes(i: Int) = Array((i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)
}
