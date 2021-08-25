package js7.common.akkautils

import akka.util.ByteString
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops._

object ByteStrings
{
  object syntax
  {
    implicit val byteStringByteSequence: ByteSequence[ByteString] =
      ByteStringByteSequence

    implicit final class ByteStringToByteSequence[A](private val byteSeq: A) extends AnyVal
    {
      def toByteString(implicit A: ByteSequence[A]) =
        byteSeq.toByteSequence(byteStringByteSequence)
    }
  }
}
