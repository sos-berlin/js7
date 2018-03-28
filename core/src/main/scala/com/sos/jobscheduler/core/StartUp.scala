package com.sos.jobscheduler.core

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations
import java.io.File
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object StartUp {
  private val logger = Logger(getClass)

  /** Log Java version, config and data directory, and classpath. */
  def logStartUp(configDir: Path, dataDir: Path): Unit = {
    logger.info(
      s"Java ${JavaInformations.implementationVersion} · " +
      s"config=$configDir " +
      s"data=$configDir")

    for (o ← sys.props("java.class.path") split File.pathSeparator) logger.debug(s"Classpath $o")
    //getClass.getClassLoader match {
    //  case cl: URLClassLoader ⇒ for ((u, i) ← cl.getURLs.zipWithIndex) logger.debug(s"ClassLoader: #${i+1} $u")
    // Java 10: jdk.internal.loader.ClassLoaders$AppClassLoader@1de0aca6
    //  case o ⇒ logger.debug(s"getClassLoader=$o ?")
    //}
  }
}
