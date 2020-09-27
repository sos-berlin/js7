package js7.base.data

import js7.base.data.ByteSequence.ops._
import scala.collection.immutable.ArraySeq

final class ByteArrayTest extends ByteSequenceTester[ByteArray]
{
  "unsafeWrap, unsafeArray" in {
    val a = Array(1.toByte)
    assert(ByteArray.unsafeWrap(a).unsafeArray eq a)
  }

  "fromSeq(ArraySeq) does not copy" in {
    val array = Array[Byte](1, 2)
    val b: collection.Seq[Byte] = ArraySeq.unsafeWrapArray(array)
    assert(ByteArray.fromSeq(b).unsafeArray eq array)
  }

  "++" in {
    assert(ByteArray.empty ++ ByteArray.empty == ByteArray.empty)
    assert(ByteArray.empty ++ ByteArray(1) == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray.empty == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray(2, 3) == ByteArray(1, 2, 3))
    assert(ByteArray(1, 2) ++ ByteArray(3) == ByteArray(1, 2, 3))
  }

  "toByteArray eq" in {
    val a = ByteArray(1.toByte, 2.toByte)
    assert(a.toByteArray eq a)
  }

  "toMimeBase64" in {
    assert(ByteArray(ByteSequenceTester.mimeByte64Bytes).toMimeBase64 == ByteSequenceTester.mimeBase64string)
  }

  "toString" in {
    assert(ByteArray.empty.toString == "ByteArray.empty")
    // For more tests, see ByteSequenceTester "show"
  }
}
