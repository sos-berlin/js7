package js7.base.utils

import js7.base.data.ByteSequence
import scodec.bits.ByteVector

object ScodecUtils
{
  object syntax
  {
    implicit val byteVectorByteSequence: ByteSequence[ByteVector] =
      new ByteVectorByteSequence
  }
}
