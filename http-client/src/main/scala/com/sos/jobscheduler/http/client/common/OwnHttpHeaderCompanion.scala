package com.sos.jobscheduler.http.client.common

/**
  * @author Joacim Zschimmer
  */
trait OwnHttpHeaderCompanion {
  /**
   * Default is the simple class name, '-' allowed.
   */
  val name: String = getClass.getSimpleName.replace("$minus", "-") stripSuffix "$"  // Scala object suffix
  lazy val lowercaseName = name.toLowerCase
}
