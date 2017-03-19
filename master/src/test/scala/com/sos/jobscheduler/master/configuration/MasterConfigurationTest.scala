package com.sos.jobscheduler.master.configuration

import com.typesafe.config.ConfigFactory
import java.time.ZoneId
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class MasterConfigurationTest extends FreeSpec {

  "Empty argument list" in {
    val c = MasterConfiguration.fromCommandLine(Nil)
    assert(c.copy(config = ConfigFactory.empty) == MasterConfiguration(
      dataDirectoryOption = None,
      configDirectoryOption = None,
      webServerBindings = Vector(),
      ZoneId.systemDefault,
      akkaAskTimeout = 60.seconds,
      journalSyncOnCommit = true,
      config = ConfigFactory.empty))
  }
}
