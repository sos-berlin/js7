package js7.common.akkautils

import akka.util.ByteString
import js7.base.data.ByteSequence.ops.*
import js7.base.data.ByteSequenceTester
import js7.common.akkautils.ByteStrings.syntax.*

final class ByteStringByteSequenceTest extends ByteSequenceTester[ByteString]
{
  "toByteSequence[ByteString]" in {
    val byteString = ByteString(1.toByte, 2.toByte)
    assert(byteString.toByteSequence[ByteString] eq byteString)
  }
}
