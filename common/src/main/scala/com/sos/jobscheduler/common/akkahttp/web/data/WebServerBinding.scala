package com.sos.jobscheduler.common.akkahttp.web.data

import akka.http.scaladsl.model.{Uri => AkkaUri}
import cats.syntax.either._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.CatsUtils._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichAkkaAsUri
import java.net.{InetAddress, InetSocketAddress}

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: WebServerBinding.Scheme
  def mutual: Boolean

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
    def mutual = false
    def toWebServerPort = WebServerPort.Http(address)

    override def toString = s"http://${address.getAddress.getHostAddress}:${address.getPort}"
  }
  object Http extends Scheme {
    override def toString = "http"
  }

  final case class Https(address: InetSocketAddress, keyStoreRef: KeyStoreRef, trustStoreRef: Option[TrustStoreRef] = None,  mutual: Boolean)
  extends WebServerBinding {
    def scheme = Https
    def toWebServerPort = WebServerPort.Https(address, mutual)

    override def toString = s"https://${address.getAddress.getHostAddress}:${address.getPort}, $keyStoreRef" +
      (if (mutual) " (client certificate required)" else "")
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
