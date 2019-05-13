package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.data.master.MasterId
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Paths
import java.time.ZoneId
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class MasterConfigurationTest extends FreeSpec {

  private val configuration = MasterConfiguration.fromCommandLine(CommandLineArguments(
    Vector("-config-directory=CONFIG", "-data-directory=DATA")))

  "Empty argument list" in {
    assert(configuration.copy(config = ConfigFactory.empty) == MasterConfiguration(
      masterId = MasterId("Master"),
      dataDirectory = Paths.get("DATA").toAbsolutePath,
      configDirectory = Paths.get("CONFIG").toAbsolutePath,
      webServerPorts = Nil,
      ZoneId.systemDefault,
      akkaAskTimeout = 60.seconds,
      name = MasterConfiguration.DefaultName,
      config = ConfigFactory.empty))
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
      CommandLineArguments(Vector("-config-directory=CONFIG", "-data-directory=DATA") ++ args),
      ConfigFactory.parseString("user.name = MasterConfigurationTest"/*Will be overridden*/))
}
