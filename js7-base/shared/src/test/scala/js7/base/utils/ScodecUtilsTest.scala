package js7.base.utils

import js7.base.data.ByteSequence.ops._
import js7.base.data.{ByteSequence, ByteSequenceTester}
import js7.base.utils.InputStreams.inputStreamToByteVector
import js7.base.utils.ScodecUtils.syntax._
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class ScodecUtilsTest extends ByteSequenceTester[ByteVector]
{
  "toInputStream" in {
    for (size <- Iterator(0, 1, 10000, 100000)) {
      val byteVector = ByteSequence[ByteVector].random(size)
      assert(inputStreamToByteVector(byteVector.toInputStream) == byteVector)
    }
  }
}
