package js7.common.akkahttp.web.data

import cats.syntax.show.*
import java.net.InetSocketAddress
import js7.common.internet.IP.inetSocketAddressShow

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerPort {
  def scheme: WebServerBinding.Scheme
  def address: InetSocketAddress

  override def toString = s"$scheme://${address.show}"
}

object WebServerPort
{
  def localhost(port: Int): Http =
    Http(new InetSocketAddress("127.0.0.1", port))

  final case class Http(address: InetSocketAddress) extends WebServerPort {
    def scheme = WebServerBinding.Http
  }

  final case class Https(address: InetSocketAddress) extends WebServerPort {
    def scheme = WebServerBinding.Https
  }
}
