package js7.common.pekkoutils

import js7.base.data.ByteSequence.ops.*
import js7.base.data.ByteSequenceTester
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.util.ByteString

final class ByteStringByteSequenceTest extends ByteSequenceTester[ByteString]:
  "toByteSequence[ByteString]" in:
    val byteString = ByteString(1.toByte, 2.toByte)
    assert(byteString.toByteSequence[ByteString] eq byteString)
