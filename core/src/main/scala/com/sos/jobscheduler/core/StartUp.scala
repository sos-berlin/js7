package com.sos.jobscheduler.core

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
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
      s"config=$configDir " +
      s"Java " + JavaInformations.implementationVersion + " " +
      "(" + toMB(sys.runtime.maxMemory) + ") · " +
      sys.props("os.name") + (distributionNameAndVersionOption.map(o ⇒ s" ($o)").getOrElse("")) + " · " +
      cpuModel.map(o ⇒ s"$o, ").getOrElse("") + sys.runtime.availableProcessors + " threads · " +
      (if (hostname.nonEmpty) s"host=$hostname " else "") +
      s"data=$dataDir")

    for (o ← sys.props("java.class.path") split File.pathSeparator) {
      logger.debug(Logger.Java, s"Classpath $o")
    }
  }
}
