package js7.core.configuration

import cats.syntax.semigroup._
import com.typesafe.config.Config
import java.net.InetSocketAddress
import java.nio.file.Path
import js7.base.convert.As
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import js7.common.commandline.CommandLineArguments
import js7.common.internet.IP.StringToServerInetSocketAddress
import js7.common.scalautil.Logger
import js7.core.configuration.CommonConfiguration._

/**
  * @author Joacim Zschimmer
  */
trait CommonConfiguration extends WebServerBinding.HasLocalUris
{
  def config: Config

  def configDirectory: Path

  def webServerPorts: Seq[WebServerPort]

  final lazy val keyStoreRef: Checked[KeyStoreRef] =
    KeyStoreRef.fromConfig(config, default = configDirectory resolve "private/https-keystore.p12")

  final lazy val keyStoreRefOption: Option[KeyStoreRef] =
    keyStoreRef onProblem (p => logger.debug(s"No keystore: $p"))

  final lazy val trustStoreRefs: Seq[TrustStoreRef] =
    TrustStoreRef.fromConfig(config)

  final def http: Seq[WebServerBinding.Http] =
    webServerBindings collect { case o: WebServerBinding.Http => o }

  final def https: Seq[WebServerBinding.Https] =
    webServerBindings collect { case o: WebServerBinding.Https => o }

  final def webServerBindings: Seq[WebServerBinding] =
    webServerPorts map {
      case WebServerPort.Http(port) =>
        WebServerBinding.Http(port)

      case WebServerPort.Https(port, mutual) =>
        WebServerBinding.Https(port,
          keyStoreRef.mapProblem(Problem("HTTPS requires a key store:") |+| _).orThrow,
          trustStoreRefs,
          mutual = mutual)
    }
}

object CommonConfiguration
{
  private val logger = Logger(getClass)

  private val AddressAndMutual: As[String, (InetSocketAddress, Boolean)] =
    string => (StringToServerInetSocketAddress.apply(string stripSuffix ",mutual"), string endsWith ",mutual")

  final case class Common(
    configDirectory: Path,
    dataDirectory: Path,
    webServerPorts: Seq[WebServerPort])

  object Common {
    def fromCommandLineArguments(a: CommandLineArguments): Common =
      Common(
        dataDirectory = a.as[Path]("-data-directory=").toAbsolutePath,
        configDirectory = a.as[Path]("-config-directory=").toAbsolutePath,
        webServerPorts =
          a.seqAs("-http-port=")(StringToServerInetSocketAddress).map(WebServerPort.Http) ++
          a.seqAs("-https-port=")(AddressAndMutual).map {
            case (address, mutual) => WebServerPort.Https(address, mutual = mutual)
          })
  }
}
