package js7.controller.configuration

import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.time.ZoneId
import js7.base.time.ScalaTime._
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.cluster.ClusterConf
import js7.controller.configuration.ControllerConfiguration.DefaultConfig
import js7.core.event.journal.JournalConf
import js7.data.cluster.ClusterNodeId
import js7.data.controller.ControllerId
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerConfigurationTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private lazy val directory = createTempDirectory("ControllerConfigurationTest-")

  override def beforeAll() = {
    createDirectories(directory / "DATA" / "state")
  }

  override def afterAll() = {
    delete(directory / "DATA/state")
    delete(directory / "DATA")
  }

  private lazy val configuration = ControllerConfiguration.fromCommandLine(CommandLineArguments(
    Vector(s"--config-directory=$directory/CONFIG", s"--data-directory=$directory/DATA")))

  "Empty argument list" in {
    assert(configuration.copy(config = DefaultConfig) == ControllerConfiguration(
      controllerId = ControllerId("Controller"),
      dataDirectory = (directory / "DATA").toAbsolutePath,
      configDirectory = (directory /"CONFIG").toAbsolutePath,
      webServerPorts = Nil,
      ZoneId.systemDefault,
      akkaAskTimeout = 1.h,
      journalConf = JournalConf.fromConfig(DefaultConfig)
        .copy(slowCheckState = true/*set by build.sbt*/),
      clusterConf = ClusterConf(ClusterNodeId("Primary"), isBackup = false, None, None,
        RecouplingStreamReaderConf(
          timeout = 6500.ms,  // Between 3s and 10s
          delay = 1.s),
        heartbeat = 3.s,
        failAfter = 10.s,
        Nil),
      name = ControllerConfiguration.DefaultName,
      config = DefaultConfig))
  }

  "--id=" in {
    assert(conf().controllerId == ControllerId("Controller"))
    assert(conf("--id=CONTROLLER").controllerId == ControllerId("CONTROLLER"))
  }

  "--http-port=" in {
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { conf("--http-port=65536") }
    assert(conf("--http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
  }

  "--https-port=" in {
    // For more tests see CommonConfigurationTest
    assert(conf("--https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234), mutual = false) :: Nil)
  }

  "System property" in {
    assert(conf().config.getString("user.name") == sys.props("user.name"))
  }

  private def conf(args: String*) =
    ControllerConfiguration.fromCommandLine(
      CommandLineArguments(Vector(s"--config-directory=$directory/CONFIG", s"--data-directory=$directory/DATA") ++ args),
      ConfigFactory.parseString("user.name = ControllerConfigurationTest"/*Will be overridden*/))
}
