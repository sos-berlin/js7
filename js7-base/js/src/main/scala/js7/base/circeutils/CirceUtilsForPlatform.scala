package js7.base.circeutils

import io.circe.{Json, ParsingFailure}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

private[circeutils] object CirceUtilsForPlatform
{
  private[circeutils] def parseJsonByteBuffer(buffer: ByteBuffer): Either[ParsingFailure, Json] =
    io.circe.parser.parse(new String(buffer.array, UTF_8))

  private[circeutils] def parseJsonByteArray(bytes: Array[Byte]): Either[ParsingFailure, Json] =
    io.circe.parser.parse(new String(bytes, UTF_8))
}
