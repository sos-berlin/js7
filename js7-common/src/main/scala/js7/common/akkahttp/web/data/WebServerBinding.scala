package js7.common.akkahttp.web.data

import akka.http.scaladsl.model.{Uri => AkkaUri}
import cats.syntax.either._
import java.net.{InetAddress, InetSocketAddress}
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils._
import js7.base.web.Uri
import js7.common.http.AkkaHttpUtils.RichAkkaAsUri

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding
{
  def address: InetSocketAddress
  def scheme: WebServerBinding.Scheme

  def toWebServerPort: WebServerPort
}

object WebServerBinding
{
  def http(port: Int): Http =
    WebServerBinding.Http(new InetSocketAddress("0.0.0.0", port))

  sealed trait Scheme

  final case class Http(address: InetSocketAddress)
  extends WebServerBinding {
    def scheme = Http
    def toWebServerPort = WebServerPort.Http(address)

    override def toString = s"http://${address.getAddress.getHostAddress}:${address.getPort}"
  }
  object Http extends Scheme {
    override def toString = "http"
  }

  final case class Https(
    address: InetSocketAddress,
    keyStoreRef: KeyStoreRef,
    trustStoreRefs: Seq[TrustStoreRef] = Nil)
  extends WebServerBinding {
    def scheme = Https
    def toWebServerPort = WebServerPort.Https(address)

    override def toString = s"https://${address.getAddress.getHostAddress}:${address.getPort} ($keyStoreRef" +
      ", " + (trustStoreRefs.map(_.toString).mkString(", ")) + ")"
  }
  object Https extends Scheme {
    override def toString = "https"
  }

  trait HasLocalUris
  {
    protected def webServerPorts: Seq[WebServerPort]

    final lazy val localHttpUri: Checked[Uri] = locallyUsableUri(WebServerBinding.Http)
    final lazy val localHttpsUri: Checked[Uri] = locallyUsableUri(WebServerBinding.Https)
    final def localUri: Uri = (localHttpUri.toValidated findValid localHttpsUri.toValidated).orThrow

    final def locallyUsableUri(scheme: WebServerBinding.Scheme): Checked[Uri] =
      webServerPorts.collectFirst { case o if o.scheme == scheme => toLocallyUsableUri(scheme, o.address) }
        .toChecked(Problem(s"No locally usable '$scheme' address: $webServerPorts"))

    private def toLocallyUsableUri(scheme: WebServerBinding.Scheme, address: InetSocketAddress): Uri = {
      val localhost = scheme match {
        case WebServerBinding.Http =>
          if (Set("0.0.0.0", "127.0.0.1") contains address.getAddress.getHostAddress)
            "127.0.0.1"
          else
            address.getAddress.getHostAddress

        case WebServerBinding.Https =>
          assertThat(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")  // Check file /etc/host
          "localhost"  // To match TLS host name verification
      }
      val host = address.getAddress.getHostAddress match {
        case "0.0.0.0" | "127.0.0.1" => localhost
        case o => o
      }
      val port = address.getPort
      AkkaUri(scheme.toString, AkkaUri.Authority(AkkaUri.Host(host), port)).asUri
    }
  }
}
