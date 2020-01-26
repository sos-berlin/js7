package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.event.journal.JournalConf
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.cluster.ClusterConf
import com.sos.jobscheduler.master.configuration.MasterConfiguration.DefaultConfig
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.time.ZoneId
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class MasterConfigurationTest extends FreeSpec with BeforeAndAfterAll
{
  private lazy val directory = createTempDirectory("MasterConfigurationTest-")

  override def beforeAll() = {
    createDirectories(directory / "DATA" / "state")
  }

  override def afterAll() = {
    delete(directory / "DATA/state")
    delete(directory / "DATA")
  }

  private lazy val configuration = MasterConfiguration.fromCommandLine(CommandLineArguments(
    Vector(s"-config-directory=$directory/CONFIG", s"-data-directory=$directory/DATA")))

  "Empty argument list" in {
    assert(configuration.copy(config = DefaultConfig) == MasterConfiguration(
      masterId = MasterId("Master"),
      dataDirectory = (directory / "DATA").toAbsolutePath,
      configDirectory = (directory /"CONFIG").toAbsolutePath,
      webServerPorts = Nil,
      ZoneId.systemDefault,
      akkaAskTimeout = 60.s,
      journalConf = JournalConf.fromConfig(DefaultConfig),
      clusterConf = ClusterConf(None, None, None,
        RecouplingStreamReaderConf(
          timeout = 55.s,
          delay = 1.s),
        heartbeat = 1.s,
        failAfter = 5.s),
      name = MasterConfiguration.DefaultName,
      config = DefaultConfig))
  }

  "-id=" in {
    assert(conf().masterId == MasterId("Master"))
    assert(conf("-id=MASTER").masterId == MasterId("MASTER"))
  }

  "-http-port=" in {
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { conf("-http-port=65536") }
    assert(conf("-http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
  }

  "-https-port=" in {
    // For more tests see CommonConfigurationTest
    assert(conf("-https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234), mutual = false) :: Nil)
  }

  "System property" in {
    assert(conf().config.getString("user.name") == sys.props("user.name"))
  }

  private def conf(args: String*) =
    MasterConfiguration.fromCommandLine(
      CommandLineArguments(Vector(s"-config-directory=$directory/CONFIG", s"-data-directory=$directory/DATA") ++ args),
      ConfigFactory.parseString("user.name = MasterConfigurationTest"/*Will be overridden*/))
}
