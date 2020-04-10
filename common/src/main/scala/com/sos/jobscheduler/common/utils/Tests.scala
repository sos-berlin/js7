package com.sos.jobscheduler.common.utils

import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
object Tests
{
  lazy val isTest: Boolean = Thread.getAllStackTraces.keySet.asScala exists (_.getName startsWith "ScalaTest-")
}
