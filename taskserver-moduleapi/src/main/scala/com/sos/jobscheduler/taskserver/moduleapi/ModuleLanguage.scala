package com.sos.jobscheduler.taskserver.moduleapi

import com.sos.jobscheduler.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class ModuleLanguage(string: String) extends IsString {
  if (string exists { _.isUpper }) throw new IllegalArgumentException(s"Not completely lower case: language='$string'")
}
