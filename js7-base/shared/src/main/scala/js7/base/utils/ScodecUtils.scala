package js7.base.utils

import io.circe.{Decoder, Json}
import java.nio.charset.CodingErrorAction.REPLACE
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import scala.util.Random
import scodec.bits.ByteVector

object ScodecUtils
{
  object syntax
  {
    implicit final class RichByteVectorCompanion(private val underlying: ByteVector.type) extends AnyVal
    {
      def random(size: Int): ByteVector = {
        val bytes = new Array[Byte](size)
        Random.nextBytes(bytes)
        ByteVector.view(bytes)
      }
    }
  }

  implicit final class RichByteVector(private val underlying: ByteVector) extends AnyVal
  {
    def utf8StringTruncateAt(truncateAt: Int): String =  // TODO Truncate big ByteVector before decoding
      utf8String.truncateWithEllipsis(truncateAt)

    def utf8String: String =
      UTF_8.newDecoder()
        .onMalformedInput(REPLACE)
        .onUnmappableCharacter(REPLACE)
        .decode(underlying.toByteBuffer)
        .toString

    def indexOf(byte: Byte): Int =
      indexOf(byte, 0)

    def indexOf(byte: Byte, start: Int): Int =
      indexOf(byte, start.toLong).toInt

    def indexOf(byte: Byte, start: Long): Long = {
      var i = start
      val length = underlying.length
      while (i < length) {
        if (underlying.get(i) == byte) return i
        i += 1
      }
      -1
    }

    def toInputStream = new ByteVectorInputStream(underlying)

    def parseJsonAs[A: Decoder]: Checked[A] =
      parseJson flatMap (_.checkedAs[A])

    def parseJson: Checked[Json] =
      underlying.decodeUtf8.toThrowableChecked
        .flatMap(string => io.circe.parser.parse(string).toChecked)
  }
}
