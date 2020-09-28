package js7.common.akkautils

import akka.util.ByteString
import js7.base.data.{ByteArray, ByteSequence}

object ByteStrings
{
  object syntax
  {
    implicit val byteVectorByteSequence: ByteSequence[ByteString] =
      new ByteStringByteSequence

    implicit final class RichByteString(private val underlying: ByteString) extends AnyVal
    {
      def toByteArray = ByteArray.unsafeWrap(underlying.toArray)
    }
  }
}
