package js7.base.utils

import js7.base.convert.As.StringAsBoolean
import scala.sys.SystemProperties

object SystemPropertiesExtensions:

  extension (properties: SystemProperties)
    def asSwitch(key: String): Boolean =
      properties.get(key) match
        case None => false
        case Some("") => true
        case Some(string) => StringAsBoolean(string)
