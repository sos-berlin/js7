package com.sos.jobscheduler.common.akkahttp.web.data

import java.net.InetSocketAddress

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerPort {
  def scheme: WebServerBinding.Scheme
  def address: InetSocketAddress
}

object WebServerPort
{
  final case class Http(address: InetSocketAddress) extends WebServerPort {
    def scheme = WebServerBinding.Http
    override def toString = s"http://${address.getAddress.getHostAddress}:${address.getPort}"
  }

  final case class Https(address: InetSocketAddress, mutual: Boolean) extends WebServerPort {
    def scheme = WebServerBinding.Https
    override def toString = s"https://${address.getAddress.getHostAddress}:${address.getPort}" +
      (if (mutual) " (client certificate required)" else "")
  }
}
