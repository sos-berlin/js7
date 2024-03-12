package js7.base.data

import js7.base.data.ByteSequence.ops.*
import scala.collection.immutable.ArraySeq

final class ByteArrayTest extends ByteSequenceTester[ByteArray]:

  "unsafeWrap, unsafeArray" in:
    val array = Array[Byte](1, 2, 3)
    assert(ByteArray.unsafeWrap(array).unsafeArray eq array)

  "fromSeq(ArraySeq) does not copy" in:
    val array = Array[Byte](1, 2)
    val seq: collection.Seq[Byte] = ArraySeq.unsafeWrapArray(array)
    assert(ByteArray.fromSeq(seq).unsafeArray eq array)

  "++" in:
    assert(ByteArray.empty ++ ByteArray.empty == ByteArray.empty)
    assert(ByteArray.empty ++ ByteArray(1) == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray.empty == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray(2, 3) == ByteArray(1, 2, 3))
    assert(ByteArray(1, 2) ++ ByteArray(3) == ByteArray(1, 2, 3))

  "toByteArray eq" in:
    val a = ByteArray(1.toByte, 2.toByte)
    assert(a.toByteArray eq a)

  "toByteSequence[ByteArray]" in:
    val byteArray = ByteArray(1.toByte, 2.toByte)
    assert(byteArray.toByteSequence[ByteArray] eq byteArray)

  "toMimeBase64" in:
    assert(ByteArray(ByteSequenceTester.mimeByte64Bytes).toMimeBase64 == ByteSequenceTester.mimeBase64string)

  "unsafeWrappedArray" in :
    val array = Array[Byte](1, 2, 3)
    assert(ByteArray.unsafeWrap(array).unsafeWrappedArray.get eq array)

  "toString" in:
    assert(ByteArray.empty.toString == "»«")
    // For more tests, see ByteSequenceTester "show"
