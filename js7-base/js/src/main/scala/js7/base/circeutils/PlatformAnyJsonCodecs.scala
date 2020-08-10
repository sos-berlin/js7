package js7.base.circeutils

import io.circe.Json

object PlatformAnyJsonCodecs
{
  val platformAnyToJson: PartialFunction[Any, Json] = PartialFunction.empty
}
