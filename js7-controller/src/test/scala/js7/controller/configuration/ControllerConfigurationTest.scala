package js7.controller.configuration

import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.time.ZoneId
import js7.base.configutils.Configs.*
import js7.base.convert.As.StringAsBoolean
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.DelayConf
import js7.cluster.ClusterConf
import js7.common.commandline.CommandLineArguments
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.pekkohttp.web.data.WebServerPort
import js7.data.cluster.ClusterTiming
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import js7.journal.configuration.JournalConf
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class ControllerConfigurationTest extends OurTestSuite, BeforeAndAfterAll:
  private lazy val directory = createTempDirectory("ControllerConfigurationTest-")

  override def beforeAll() =
    createDirectories(directory / "DATA" / "state")

  override def afterAll() =
    delete(directory / "DATA/state")
    delete(directory / "DATA")

  private lazy val configuration = ControllerConfiguration.fromCommandLine(CommandLineArguments(
    Vector(s"--config-directory=$directory/CONFIG", s"--data-directory=$directory/DATA")))

  "Empty argument list" in:
    assert(configuration.copy(
      config = ConfigFactory.empty,
      clusterConf = configuration.clusterConf.copy(
        config = ConfigFactory.empty)) ==
      ControllerConfiguration(
        controllerId = ControllerId("Controller"),
        dataDirectory = (directory / "DATA").toAbsolutePath,
        configDirectory = (directory / "CONFIG").toAbsolutePath,
        webServerPorts = Nil,
        ZoneId.systemDefault,
        pekkoAskTimeout = 1.h,
        clusterConf = ClusterConf(
          JournalConf.fromConfig(configuration.config)
            .copy(slowCheckState = sys.props.get("js7.test").fold(false)(StringAsBoolean(_))),
          NodeId("Primary"), isBackup = false, None,
          RecouplingStreamReaderConf(
            timeout = 6500.ms, // Between 3s and 10s
            keepAlive = 1.s,
            delay = 1.s,
            failureDelays = Nel.of(1.s, 3.s, 6.s, 10.s)),
          ClusterTiming(3.s, 10.s),
          clusterWatchUniquenessMemorySize = 1000,
          delayConf = DelayConf(1.s, 1.s, 1.s, 1.s, 1.s, 2.s, 3.s, 5.s),
          config = ConfigFactory.empty),
        name = ControllerConfiguration.DefaultName,
        config = ConfigFactory.empty))

  "--id=" in:
    assert(conf().controllerId == ControllerId("Controller"))
    assert(conf("--id=CONTROLLER").controllerId == ControllerId("CONTROLLER"))

  "--http-port=" in:
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { conf("--http-port=65536") }
    assert(conf("--http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "--https-port=" in:
    // For more tests see CommonConfigurationTest
    assert(conf("--https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "System property" in:
    assert(conf().config.getString("user.name") == sys.props("user.name"))

  private def conf(args: String*) =
    ControllerConfiguration.fromCommandLine(
      CommandLineArguments(Vector(s"--config-directory=$directory/CONFIG", s"--data-directory=$directory/DATA") ++ args),
      config"user.name = ControllerConfigurationTest"/*Will be overridden*/)
