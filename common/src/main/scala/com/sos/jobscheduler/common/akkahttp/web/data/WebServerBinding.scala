package com.sos.jobscheduler.common.akkahttp.web.data

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.https.KeyStoreRef
import java.net.{InetAddress, InetSocketAddress}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: WebServerBinding.Scheme
  def mutual: Boolean

  def toWebServerPort: WebServerPort
}

object WebServerBinding {

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

  final case class Https(address: InetSocketAddress, keyStoreRef: KeyStoreRef, mutual: Boolean)
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
    final lazy val localUri: Uri = (localHttpUri findValid localHttpsUri).orThrow

    final def locallyUsableUri(scheme: WebServerBinding.Scheme): Checked[Uri] =
      webServerPorts.collectFirst { case o if o.scheme == scheme ⇒ toLocallyUsableUri(scheme, o.address) }
      .toChecked(Problem(s"No locally usable '$scheme' address: $webServerPorts"))

    private def toLocallyUsableUri(scheme: WebServerBinding.Scheme, address: InetSocketAddress) = {
      val localhost = scheme match {
        case WebServerBinding.Http ⇒ "127.0.0.1"
        case WebServerBinding.Https ⇒
          assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")  // Check file /etc/host
          "localhost"
      }
      val host = address.getAddress.getHostAddress match {
        case "0.0.0.0" | "127.0.0.1" ⇒ localhost
        case o ⇒ o
      }
      val port = address.getPort
      Uri(scheme.toString, Uri.Authority(Uri.Host(host), port))
    }
  }
}
