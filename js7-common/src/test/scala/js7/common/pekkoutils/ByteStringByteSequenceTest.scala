package js7.common.pekkoutils

import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteSequence, ByteSequenceTester}
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.util.ByteString

final class ByteStringByteSequenceTest extends ByteSequenceTester[ByteString]:

  private val ByteSeq = summon[ByteSequence[ByteString]]

  "unsafeWrap, unsafeArray" in:
    val array = Array[Byte](1, 2, 3)
    assert(ByteString.fromArrayUnsafe(array).unsafeArray eq array)
    assert(ByteSeq.unsafeWrap(array).unsafeArray eq array)

  "toByteSequence[ByteString]" in:
    val byteString = ByteString(1.toByte, 2.toByte)
    assert(byteString.toByteSequence[ByteString] eq byteString)

  "toByteString eq" in:
    val a = ByteString(1.toByte, 2.toByte)
    assert(a.toByteString eq a)

  "unsafeWrappedArray" in :
    val array = Array[Byte](1, 2, 3)
    assert(ByteString.fromArrayUnsafe(array).unsafeWrappedArray.get eq array)
    assert((ByteString(1) ++ ByteString(2)).unsafeWrappedArray.isEmpty)
