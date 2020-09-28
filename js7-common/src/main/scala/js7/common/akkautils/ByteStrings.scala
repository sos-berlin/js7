package js7.common.akkautils

import akka.util.ByteString
import js7.base.data.ByteSequence.ops._
import js7.base.data.{ByteArray, ByteSequence}

object ByteStrings
{
  object syntax
  {
    implicit val byteVectorByteSequence: ByteSequence[ByteString] =
      new ByteStringByteSequence

    implicit final class RichByteString(private val underlying: ByteString) extends AnyVal {
      def toByteArray: ByteArray =
        toByteSeq(ByteArray)

      def toByteSeq[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
         ByteSeq.unsafeWrap(underlying.toArray)
    }

    implicit final class ByteStringToByteSequence[A](private val underlying: A) extends AnyVal {
      def toByteString(implicit A: ByteSequence[A]) =
        ByteString.fromArrayUnsafe(underlying.unsafeArray)
    }
  }
}
