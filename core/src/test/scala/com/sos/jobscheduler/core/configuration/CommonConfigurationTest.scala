package com.sos.jobscheduler.core.configuration

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.configuration.CommonConfigurationTest._
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class CommonConfigurationTest extends FreeSpec
{
  "-config-directory=" in {
    assert(conf().configDirectory == Paths.get("CONFIG").toAbsolutePath)
  }

  "-data-directory=" in {
    assert(conf().dataDirectory == Paths.get("DATA").toAbsolutePath)
  }

  "-http-port=" in {
    intercept[IllegalArgumentException] { conf("-http-port=65536") }
    assert(conf("-http-port=1234"              ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)
    assert(conf("-http-port=11.22.33.44:1234"  ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("11.22.33.44", 1234)) :: Nil)
    assert(conf("-http-port=[1:2:3:4:5:6]:1234").webServerBindings == WebServerBinding.Http(new InetSocketAddress("1:2:3:4:5:6", 1234)) :: Nil)
    assert(conf("-http-port=[::1]:1234"        ).webServerBindings == WebServerBinding.Http(new InetSocketAddress("::1", 1234)) :: Nil)
    assert(conf("-http-port=1111", "-http-port=2222").webServerBindings ==
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 1111)) ::
      WebServerBinding.Http(new InetSocketAddress("0.0.0.0", 2222)) :: Nil)
  }

  "-https-port=" in {
    intercept[IllegalArgumentException] {
      conf("-https-port=65536")
    }
    val config = Paths.get("CONFIG").toAbsolutePath
    assert(conf("-https-port=1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("0.0.0.0", 1234),
      KeyStoreRef(
        url = (config / "private/https-keystore.p12").toUri.toURL,
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      Some(TrustStoreRef(
        url = (config / "private/https-truststore.p12").toUri.toURL,
        storePassword = SecretString("TRUSTSTORE-PASSWORD"))),
        mutual = false)))
    assert(conf("-https-port=11.22.33.44:1234").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("11.22.33.44", 1234),
      KeyStoreRef(
        url = (config / "private/https-keystore.p12").toUri.toURL,
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      Some(TrustStoreRef(
        url = (config / "private/https-truststore.p12").toUri.toURL,
        storePassword = SecretString("TRUSTSTORE-PASSWORD"))),
        mutual = false)))
  }

  "-https-port=n,mutual" in {
    val config = Paths.get("CONFIG").toAbsolutePath
    assert(conf("-https-port=1234,mutual").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("0.0.0.0", 1234),
      KeyStoreRef(
        url = (config / "private/https-keystore.p12").toUri.toURL,
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      Some(TrustStoreRef(
        url = (config / "private/https-truststore.p12").toUri.toURL,
        storePassword = SecretString("TRUSTSTORE-PASSWORD"))),
        mutual = true)))
    assert(conf("-https-port=11.22.33.44:1234,mutual").webServerBindings == List(WebServerBinding.Https(
      new InetSocketAddress("11.22.33.44", 1234),
      KeyStoreRef(
        url = (config / "private/https-keystore.p12").toUri.toURL,
        storePassword = SecretString("KEYSTORE-STORE-PASSWORD"),
        keyPassword = SecretString("KEYSTORE-KEY-PASSWORD")),
      Some(TrustStoreRef(
        url = (config / "private/https-truststore.p12").toUri.toURL,
        storePassword = SecretString("TRUSTSTORE-PASSWORD"))),
        mutual = true)))
  }
}

private object CommonConfigurationTest {
  private case class TestConf(configDirectory: Path, dataDirectory: Path, webServerPorts: Seq[WebServerPort], config: Config)
  extends CommonConfiguration

  private def conf(args: String*) = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(CommandLineArguments(
      Vector("-config-directory=CONFIG", "-data-directory=DATA") ++ args))
    TestConf(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      webServerPorts = common.webServerPorts,
      ConfigFactory.parseString(
        """jobscheduler.https.keystore {
          |  store-password = "KEYSTORE-STORE-PASSWORD"
          |  key-password = "KEYSTORE-KEY-PASSWORD"
          |}
          |jobscheduler.https.truststore {
          |  store-password = "TRUSTSTORE-PASSWORD"
          |}
          |""".stripMargin))
  }
}
