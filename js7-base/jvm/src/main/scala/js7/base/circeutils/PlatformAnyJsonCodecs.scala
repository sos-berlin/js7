package js7.base.circeutils

import io.circe.Json
import java.nio.file.Path

object PlatformAnyJsonCodecs:
  val platformAnyToJson: PartialFunction[Any, Json] =
    case v: Path => Json.fromString(v.toString)  // Do not iterate through Path's Iterable interface
