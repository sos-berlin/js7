package js7.common.configutils

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.circeutils.CirceUtils._

object Hocon
{
  implicit final class HoconStringInterpolator(private val sc: StringContext) extends AnyVal
  {
    def hocon(args: Any*): Config =
      ConfigFactory.parseString(
        JsonStringInterpolator.interpolate(sc, args))
  }
}
