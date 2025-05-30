package js7.common.pekkoutils

import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import org.apache.pekko.util.ByteString

object ByteStrings:

  object syntax:
    implicit val byteStringByteSequence: ByteSequence[ByteString] =
      ByteStringByteSequence

    implicit final class ByteStringToByteSequence[A](private val byteSeq: A) extends AnyVal:
      def toByteString(implicit A: ByteSequence[A]): ByteString =
        byteSeq.toByteSequence(using byteStringByteSequence)
