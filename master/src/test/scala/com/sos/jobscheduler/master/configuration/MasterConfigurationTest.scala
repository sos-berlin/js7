package com.sos.jobscheduler.master.configuration

import com.typesafe.config.ConfigFactory
import java.nio.file.Paths
import java.time.ZoneId
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class MasterConfigurationTest extends FreeSpec {

  "Empty argument list" in {
    val c = MasterConfiguration.fromCommandLine(Vector("-data-directory=DATA"))
    assert(c.copy(config = ConfigFactory.empty) == MasterConfiguration(
      dataDirectory = Paths.get("DATA").toAbsolutePath,
      configDirectoryOption = None,
      webServerBindings = Vector(),
      ZoneId.systemDefault,
      akkaAskTimeout = 60.seconds,
      journalSyncOnCommit = true,
      config = ConfigFactory.empty))
  }
}
