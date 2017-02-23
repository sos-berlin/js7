package com.sos.scheduler.engine.common

import com.sos.scheduler.engine.common.scalautil.Logger

/**
 * @author Joacim Zschimmer
 */
object ClassLoaders {
  private val logger = Logger(getClass)

  def currentClassLoader: ClassLoader =
    Thread.currentThread.getContextClassLoader match {
      case null ⇒
        val o = getClass.getClassLoader
        logger.debug(s"currentThread.getContextClassLoader is null, using ${getClass.getName}.getClassLoader=$o")
        o
      case o ⇒ o
    }
}
