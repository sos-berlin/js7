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
  }

  final case class Https(address: InetSocketAddress, mutual: Boolean) extends WebServerPort {
    def scheme = WebServerBinding.Https
  }
}
