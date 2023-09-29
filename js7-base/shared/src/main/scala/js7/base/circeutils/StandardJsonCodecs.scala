package js7.base.circeutils

import io.circe.Codec
import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.{stringJsonCodec, toStringJsonCodec}
import scala.util.matching.Regex

object StandardJsonCodecs:
  implicit val PatternJsonCodec: Codec[Pattern] =
    stringJsonCodec(
      pattern =>
        if pattern.flags() != 0 then
          throw new IllegalArgumentException(s"Pattern with flags cannot be JSON-encoded: $pattern")
        else
          pattern.pattern,
      Pattern.compile(_))

  implicit val RegexJsonCodec: Codec[Regex] =
    toStringJsonCodec(_.r)
