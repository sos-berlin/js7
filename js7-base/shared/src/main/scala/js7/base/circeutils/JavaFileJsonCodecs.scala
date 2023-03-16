package js7.base.circeutils

import io.circe.Codec
import java.nio.file.{Path, Paths}
import js7.base.circeutils.CirceUtils.toStringJsonCodec

object JavaFileJsonCodecs {
  implicit val PathJsonCodec: Codec[Path] = toStringJsonCodec(o => Paths.get(o))
}
