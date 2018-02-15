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

  private val configuration = MasterConfiguration.fromCommandLine(Vector("-config-directory=CONFIG", "-data-directory=DATA"))

  "Empty argument list" in {
    assert(configuration.copy(config = ConfigFactory.empty) == MasterConfiguration(
      dataDirectory = Paths.get("DATA").toAbsolutePath,
      configDirectory = Paths.get("CONFIG").toAbsolutePath,
      webServerBindings = Vector(),
      ZoneId.systemDefault,
      akkaAskTimeout = 60.seconds,
      journalSyncOnCommit = true,
      config = ConfigFactory.empty))
  }

  "-sync-journal" in {
    assert(conf().journalSyncOnCommit)
    assert(conf("-sync-journal").journalSyncOnCommit)
    assert(conf("-sync-journal-").journalSyncOnCommit == false)
  }

  private def conf(args: String*) =
    MasterConfiguration.fromCommandLine(Vector("-config-directory=CONFIG", "-data-directory=DATA") ++ args)
}
