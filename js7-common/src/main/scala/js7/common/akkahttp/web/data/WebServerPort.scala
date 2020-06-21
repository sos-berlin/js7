package js7.common.akkahttp.web.data

import java.net.InetSocketAddress
import js7.base.utils.ScalaUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerPort {
  def scheme: WebServerBinding.Scheme
  def address: InetSocketAddress
}

object WebServerPort
{
  def localhost(port: Int): Http =
    Http(new InetSocketAddress("127.0.0.1", port))

  final case class Http(address: InetSocketAddress) extends WebServerPort {
    def scheme = WebServerBinding.Http
    override def toString = s"http://${address.getAddress.getHostAddress}:${address.getPort}"
  }

  final case class Https(address: InetSocketAddress, mutual: Boolean) extends WebServerPort {
    def scheme = WebServerBinding.Https
    override def toString = s"https://${address.getAddress.getHostAddress}:${address.getPort}" +
      (mutual ?: " (client certificate required)")
  }
}
