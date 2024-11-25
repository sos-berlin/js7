package js7.common.configuration

import com.typesafe.config.Config
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.test.OurTestSuite
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.CommonConfigurationTest.*
import js7.common.pekkohttp.web.data.{WebServerBinding, WebServerPort}

/**
  * @author Joacim Zschimmer
  */
final class CommonConfigurationTest extends OurTestSuite:
  "--config-directory=" in:
    assert(conf().configDirectory == Paths.get("CONFIG").toAbsolutePath)

  "--data-directory=" in:
    assert(conf().dataDirectory == Paths.get("DATA").toAbsolutePath)

  "--http-port=" in:
    intercept[IllegalArgumentException] { conf("--http-port=65536") }
    assert(conf("--http-port=1234"              ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
    assert(conf("--http-port=11.22.33.44:1234"  ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("11.22.33.44", 1234)) :: Nil)
    assert(conf("--http-port=[1:2:3:4:5:6]:1234").webServerBindings == WebServerBinding.Http(new InetSocketAddress("1:2:3:4:5:6", 1234)) :: Nil)
    assert(conf("--http-port=[::1]:1234"        ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("::1", 1234)) :: Nil)
    assert(conf("--http-port=1111", "--http-port=2222").webServerBindings ==
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1111)) ::
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 2222)) :: Nil)

  "--https-port=" in:
    intercept[IllegalArgumentException]:
      conf("--https-port=65536")
    val config = Paths.get("CONFIG").toAbsolutePath
    assert(conf("--https-port=1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("0.0.0.0", 1234),
      KeyStoreRef(
        (config / "private/https-keystore.p12"),
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      List(
        TrustStoreRef(
          (config / "private/https-truststore.p12"),
          storePassword = SecretString("TRUSTSTORE-PASSWORD")),
        TrustStoreRef(
          (config / "private/second-https-truststore.p12"),
          storePassword = SecretString("SECOND-TRUSTSTORE-PASSWORD"))))))
    assert(conf("--https-port=11.22.33.44:1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("11.22.33.44", 1234),
      KeyStoreRef(
        (config / "private/https-keystore.p12"),
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      List(
        TrustStoreRef(
          (config / "private/https-truststore.p12"),
          storePassword = SecretString("TRUSTSTORE-PASSWORD")),
        TrustStoreRef(
          (config / "private/second-https-truststore.p12"),
          storePassword = SecretString("SECOND-TRUSTSTORE-PASSWORD"))))))

private object CommonConfigurationTest:
  private case class TestConf(configDirectory: Path, dataDirectory: Path, webServerPorts: Seq[WebServerPort], config: Config)
  extends CommonConfiguration:
    def name = "CommonConfigurationTest"

  private def conf(args: String*): TestConf =
    val common = CommonConfiguration.CommonWithData.fromCommandLineArguments(CommandLineArguments(
      Vector("--config-directory=CONFIG", "--data-directory=DATA") ++ args))
    TestConf(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      webServerPorts = common.webServerPorts,
      config"""
        js7.web.https.keystore {
          store-password = "KEYSTORE-STORE-PASSWORD"
          key-password = "KEYSTORE-KEY-PASSWORD"
        }
        js7.web.https.truststores = [
          {
            file = "${common.configDirectory}/private/https-truststore.p12"
            store-password = "TRUSTSTORE-PASSWORD"
          }, {
            file = "${common.configDirectory}/private/second-https-truststore.p12"
            store-password = "SECOND-TRUSTSTORE-PASSWORD"
          }
        ]
        """)
