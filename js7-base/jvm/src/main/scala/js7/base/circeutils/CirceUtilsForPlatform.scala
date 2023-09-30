package js7.base.circeutils

import io.circe.jawn.JawnParser
import io.circe.{Json, ParsingFailure}
import java.nio.ByteBuffer

private[circeutils] object CirceUtilsForPlatform:

  private val jawnParser = new JawnParser()

  private[circeutils] def parseJsonByteArray(bytes: Array[Byte]): Either[ParsingFailure, Json] =
    jawnParser.parseByteArray(bytes)

  private[circeutils] def parseJsonByteBuffer(buffer: ByteBuffer): Either[ParsingFailure, Json] =
    jawnParser.parseByteBuffer(buffer)
