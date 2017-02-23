package com.sos.scheduler.engine.taskserver.moduleapi

import com.sos.scheduler.engine.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class ModuleLanguage(string: String) extends IsString {
  if (string exists { _.isUpper }) throw new IllegalArgumentException(s"Not completely lower case: language='$string'")
}
