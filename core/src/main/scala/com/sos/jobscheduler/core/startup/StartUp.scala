package com.sos.jobscheduler.core.startup

import com.sos.jobscheduler.common.process.ProcessPidRetriever.maybeOwnPid
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import java.io.File
import java.nio.file.Path
import monix.execution.atomic.AtomicBoolean

/**
  * @author Joacim Zschimmer
  */
object StartUp {
  private val logger = Logger(getClass)
  private val classPathLogged = AtomicBoolean(false)

  /** Log Java version, config and data directory, and classpath. */
  def logStartUp(configDir: Path, dataDir: Option[Path]): Unit = {
    logger.info(
      s"Java " + JavaInformations.implementationVersion + " " +
      "(" + toMB(sys.runtime.maxMemory) + ") · " +
      sys.props("os.name") + distributionNameAndVersionOption.fold("")(o => s" ($o)") + " · " +
      cpuModel.fold("")(o => s"$o ") + "(" + sys.runtime.availableProcessors + " threads) · " +
      (maybeOwnPid.fold("")(pid => s"pid=${pid.number} ")) +
      (if (hostname.nonEmpty) s"host=$hostname " else "") +
      s"config=$configDir " +
        dataDir.fold("")("data=".+))

    if (!classPathLogged.getAndSet(true)) {  // Log only once (for tests running master and agents in same JVM)
      val paths = sys.props("java.class.path") split File.pathSeparator filter (_.nonEmpty)
      logger.debug(Logger.Java, s"Classpath contains ${paths.size} libraries:")
      for (o <- paths) {
        logger.debug(Logger.Java, s"Classpath $o")
      }
    }
    logger.whenTraceEnabled {
      logger.debug("Logger TRACE enabled")
    }
  }
}
